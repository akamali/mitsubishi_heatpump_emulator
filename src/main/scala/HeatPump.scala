import com.fazecast.jSerialComm.SerialPort

import java.io.{DataInputStream, FileInputStream}
import java.time.{Instant, LocalDateTime, ZoneId}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

trait CommunicationInterface {
  def bytesAvailable(): Int
  def readBytes(buffer: Array[Byte], bytesToRead: Int): Int
  def sendBytes(buffer: Array[Byte]): Int
}

class SerialCommunication(serialPort: SerialPort) extends CommunicationInterface {
  override def bytesAvailable(): Int = serialPort.bytesAvailable()

  override def readBytes(buffer: Array[Byte], bytesToRead: Int): Int = serialPort.readBytes(buffer, bytesToRead)

  override def sendBytes(buffer: Array[Byte]): Int = serialPort.writeBytes(buffer, buffer.length)
}

class ArrayCommunication(_data: Array[Int], chunkSize: Int = Int.MaxValue) extends CommunicationInterface {
  private val data = _data.map { _.toByte }
  private var read = 0

  val writtenBytes = scala.collection.mutable.ArrayBuffer[Byte]()

  override def bytesAvailable(): Int = {
    if (read < data.length)
      Math.min(data.length - read, chunkSize)
    else
      -1
  }

  override def readBytes(buffer: Array[Byte], bytesToRead: Int): Int = {
    if (read >= data.length)
      return -1

    val remaining = data.length - read
    val toRead = Math.min(remaining, bytesToRead)
    System.arraycopy(data, read, buffer, 0, toRead)
    read += toRead
    toRead
  }

  override def sendBytes(buffer: Array[Byte]): Int = {
    writtenBytes ++= buffer
    buffer.length
  }
}

object Packet {
  val MAGIC = 0xfc

  def parsePacket(rawPacket: Array[Int]): Option[Packet] = {
    val byteArray: Array[Byte] = rawPacket.map { _.toByte }
    parsePacket(byteArray)
  }

  def parsePackets(raw: Array[Byte]): IndexedSeq[Packet] = {
    if (raw.isEmpty)
      return IndexedSeq.empty

    val packets = ArrayBuffer[Array[Byte]]()
    val buffer = ArrayBuffer[Byte]()
    var i = 0
    if (raw(0) != Packet.MAGIC.toByte)
      throw new Exception("Expecting nicely formatted bytes/packets here")

    while (i < raw.length) {
      if (i == 0 || raw(i) != Packet.MAGIC.toByte) {
        buffer += raw(i)
      } else {
        packets += buffer.toArray
        buffer.clear()
        buffer += raw(i)
      }
      i += 1
    }

    if (buffer.nonEmpty)
      packets += buffer.toArray

    packets.flatMap(parsePacket).toIndexedSeq
  }

  def parsePacket(rawPacket: Array[Byte]): Option[Packet] = {
    val checksum = rawPacket(rawPacket.length - 1)
    val expectedChecksum = Packet.MAGIC - rawPacket.take(rawPacket.length - 1).sum & 0xff
    if (expectedChecksum.toByte != checksum || rawPacket.length < 5) {
      //println(s"Bad checksum $expectedChecksum != $checksum")
      return Some(new BadPacket())
    }

    val unknown1 = rawPacket(2)
    val unknown2 = rawPacket(3)
    val packetLength = rawPacket(4)
    val data: Array[Int] = rawPacket.drop(5).map { b => b.toInt & 0xff }.take(packetLength)
    val packetType = PacketType.fromInt(rawPacket(1))
    packetType match {
      case PacketType.ConnectRequest =>
        Some(ConnectRequestPacket(unknown1, unknown2))

      case PacketType.ConnectResponse =>
        Some(ConnectResponsePacket(unknown1, unknown2))

      case PacketType.ExtendedConnectRequest =>
        Some(ExtendedConnectRequestPacket(unknown1, unknown2))

      case PacketType.ExtendedConnectResponse =>
        Some(ExtendedConnectResponsePacket(unknown1, unknown2))

      case PacketType.SetRequest =>
        Some(SetRequestPacket(unknown1, unknown2, Setting.getSettingsFromPayLoad(packetType, data)))

      case PacketType.SetResponse =>
        Some(SetResponsePacket(unknown1, unknown2))

      case PacketType.GetRequest =>
        //val mode: GetRequestMode.Type = GetRequestMode.values.find { _.id == data(0) }.getOrElse(throw new Exception(s"Invalid get request mode received ${data(0)}"))
        val mode: GetRequestMode.Type = GetRequestMode.values.find { _.id == data(0) }.getOrElse(GetRequestMode.Unknown)
        Some(GetRequestPacket(unknown1, unknown2, mode))

      case PacketType.GetResponse =>
        Some(GetResponsePacket(unknown1, unknown2, Setting.getSettingsFromPayLoad(packetType, data)))

      case _ =>
        Some(new BadPacket())
    }
  }
}

object PacketType extends Enumeration {
  type Type = Value
  val Unknown: PacketType.Value = Value(-1)
  val SetRequest: PacketType.Value = Value(0x41)
  val SetResponse: PacketType.Value = Value(0x61)
  val GetRequest: PacketType.Value = Value(0x42)
  val GetResponse: PacketType.Value = Value(0x62)
  val ConnectRequest: PacketType.Value = Value(0x5a)
  val ConnectResponse: PacketType.Value = Value(0x7a)
  val ExtendedConnectRequest: PacketType.Value = Value(0x5b)
  val ExtendedConnectResponse: PacketType.Value = Value(0x7b)

  def fromInt(v: Int): PacketType.Value = {
    PacketType.values.find { _.id == v }.getOrElse(Unknown)
  }
}
trait Setting

abstract class AbstractSetting[E <: Enumeration, T](enum: E, name: String, init: Int) extends Setting {
  private var value: E#Value =  enum.values.find { _.id == init }.getOrElse(throw new Exception(s"Unknown $name value $init"))
  def get: E#Value = value
  def set(newVal: E#Value): Unit = value = newVal

  override def toString: String = s"$name: ${value.toString}"
}


object Setting {
  def validateOrSetType(t: Int, data: Array[Int]): Unit = {
    data(0) match {
      case 0x00 =>
        data(0) = t
      case _ if data(0) == t =>
      case _ =>
        throw new Exception(s"Can't change this setting when data(0) isn't $t")
    }
  }

  def checkBit(v: Int, bit: Int): Boolean = (v & bit) == bit

  def getSettingsFromPayLoad(packetType: PacketType.Type, data: Array[Int]): IndexedSeq[Setting] = {
    val readers = if (packetType == PacketType.SetRequest) {
      IndexedSeq(
        PowerSetting.fromSetPayload _,
        ModeSetting.fromSetPayload _,
        FanSetting.fromSetPayload _,
        TempSetting.fromSetPayload _,
        VaneSetting.fromSetPayload _,
        WideVaneSetting.fromSetPayload _,
        RemoteTempSetting.fromSetPayload _,
        FunctionSetting.fromSetPayload _,
      )
    } else {
      IndexedSeq(
        PowerSetting.fromGetPayload _,
        ModeSetting.fromGetPayload _,
        FanSetting.fromGetPayload _,
        TempSetting.fromGetPayload _,
        VaneSetting.fromGetPayload _,
        WideVaneSetting.fromGetPayload _,
        RoomTempSetting.fromGetPayload _,
        RemoteTempSetting.fromGetPayload _,
        OperatingSetting.fromGetPayload _,
        CompressorFrequencySetting.fromGetPayload _,
        StandBySetting.fromGetPayload _,
        ErrorSetting.fromGetPayload _,
        TimersSetting.fromGetPayload _,
        FunctionSetting.fromGetPayload _,
      )
    }

    readers.flatMap { sr =>
      sr(data)
    }
  }
}


object PowerSettingValues extends Enumeration {
  type Type = Value
  val Off: Type = Value(0x00)
  val On: Type = Value(0x01)
}

case class PowerSetting(init: Int) extends AbstractSetting(PowerSettingValues, "Power", init) {
  def this(v: PowerSettingValues.Type) = this(v.id)
}

object PowerSetting {
  private val bit = 0x01
  private val byteIndex = 3

  def fromGetPayload(data: Array[Int]): Option[Setting] = {
    if (data(0) != 0x02)
      return None

    Some(new PowerSetting(data(byteIndex)))
  }

  def toGetPayload(s: PowerSetting, data: Array[Int]): Unit = {
    Setting.validateOrSetType(0x02, data)
    data(byteIndex) = s.get.id
  }

  def fromSetPayload(data: Array[Int]): Option[Setting] = {
    if (data(0) != 0x01 || !Setting.checkBit(data(1), bit))
      return None

    Some(new PowerSetting(data(byteIndex)))
  }

  def toSetPayload(s: PowerSetting, data: Array[Int]): Unit = {
    Setting.validateOrSetType(0x01, data)
    data(1) |= bit
    data(byteIndex) = s.get.id
  }
}

object ModeSettingValues extends Enumeration {
  type Type = Value
  val Heat: ModeSettingValues.Type = Value(0x01)
  val Dry: ModeSettingValues.Type = Value(0x02)
  val Cool: ModeSettingValues.Type = Value(0x03)
  val Fan: ModeSettingValues.Type = Value(0x07)
  val Auto: ModeSettingValues.Type = Value(0x08)
}

case class ModeSetting(init: Int) extends AbstractSetting(ModeSettingValues, "Mode", init) {
  def this(v: ModeSettingValues.Type) = this(v.id)
}

object ModeSetting {
  private val bit = 0x02
  private val byteIndex = 4

  def fromGetPayload(data: Array[Int]): Option[Setting] = {
    if (data(0) != 0x02)
      return None

    val iSee = if ((data(byteIndex) & 0xff) > 0x08) true else false
    val v = if (iSee) data(byteIndex) - 0x08 else data(byteIndex)

    Some(new ModeSetting(v))
  }

  def toGetPayload(s: ModeSetting, data: Array[Int]): Unit = {
    Setting.validateOrSetType(0x02, data)
    data(byteIndex) = s.get.id
  }

  def fromSetPayload(data: Array[Int]): Option[Setting] = {
    if (data(0) != 0x01 || !Setting.checkBit(data(1), bit))
      return None

    Some(new ModeSetting(data(byteIndex)))
  }

  def toSetPayload(s: ModeSetting, data: Array[Int]): Unit = {
    Setting.validateOrSetType(0x01, data)
    data(1) |= bit
    data(byteIndex) = s.get.id
  }
}

object FanSettingValues extends Enumeration {
  type Type = Value
  val Auto: Type = Value(0x00)
  val Quiet: Type = Value(0x01)
  val F1: Type = Value(0x02)
  val F2: Type = Value(0x03)
  val F3: Type = Value(0x05)
  val F4: Type = Value(0x06)
}

case class FanSetting(init: Int) extends AbstractSetting(FanSettingValues, "Fan", init) {
  def this(v: FanSettingValues.Type) = this(v.id)
}

object FanSetting {
  private val bit = 0x08
  private val byteIndex = 6

  def fromGetPayload(data: Array[Int]): Option[Setting] = {
    if (data(0) != 0x02)
      return None

    Some(new FanSetting(data(byteIndex)))
  }

  def toGetPayload(s: FanSetting, data: Array[Int]): Unit = {
    Setting.validateOrSetType(0x02, data)
    data(byteIndex) = s.get.id
  }

  def fromSetPayload(data: Array[Int]): Option[Setting] = {
    if (data(0) != 0x01 || !Setting.checkBit(data(1), bit))
      return None

    Some(new FanSetting(data(byteIndex)))
  }

  def toSetPayload(s: FanSetting, data: Array[Int]): Unit = {
    Setting.validateOrSetType(0x01, data)
    data(1) |= bit
    data(byteIndex) = s.get.id
  }
}

object VaneSettingValues extends Enumeration {
  type Type = Value
  val Auto: Type = Value(0x00)
  val F1: Type = Value(0x01)
  val F2: Type = Value(0x02)
  val F3: Type = Value(0x03)
  val F4: Type = Value(0x04)
  val F5: Type = Value(0x05)
  val Swing: Type = Value(0x07)
}

case class VaneSetting(init: Int) extends AbstractSetting(VaneSettingValues, "Vane", init) {
  def this(v: VaneSettingValues.Type) = this(v.id)
}

object VaneSetting {
  private val bit = 0x10
  private val byteIndex = 7

  def fromGetPayload(data: Array[Int]): Option[Setting] = {
    if (data(0) != 0x02)
      return None

    Some(new VaneSetting(data(byteIndex)))
  }

  def toGetPayload(s: VaneSetting, data: Array[Int]): Unit = {
    Setting.validateOrSetType(0x02, data)
    data(byteIndex) = s.get.id
  }

  def fromSetPayload(data: Array[Int]): Option[Setting] = {
    if (data(0) != 0x01 || !Setting.checkBit(data(1), bit))
      return None

    Some(new VaneSetting(data(byteIndex)))
  }

  def toSetPayload(s: VaneSetting, data: Array[Int]): Unit = {
    Setting.validateOrSetType(0x01, data)
    data(1) |= bit
    data(byteIndex) = s.get.id
  }
}

object WideVaneSettingValues extends Enumeration {
  type Type = Value
  val Unknown: Type = Value(0x00)
  val LL: Type = Value(0x01)
  val L: Type = Value(0x02)
  val Mid: Type = Value(0x03)
  val R: Type = Value(0x04)
  val RR: Type = Value(0x05)
  val LR: Type = Value(0x08)
  val Swing: Type = Value(0x0C)
}

case class WideVaneSetting(init: Int, val adjust: Boolean) extends AbstractSetting(WideVaneSettingValues, "WideVane", init) {
  def this(v: WideVaneSettingValues.Type, adjust: Boolean) = this(v.id, adjust)
}

object WideVaneSetting {
  private val bit = 0x01
  private val getByteIndex = 10
  private val setByteIndex = 13

  def fromGetPayload(data: Array[Int]): Option[Setting] = {
    if (data(0) != 0x02)
      return None

    val wv = data(getByteIndex) & 0x0f
    val adjust = if ((data(getByteIndex) & 0xF0) == 0x80) true else false
    Some(new WideVaneSetting(wv, adjust))
  }

  def toGetPayload(s: WideVaneSetting, data: Array[Int]): Unit = {
    Setting.validateOrSetType(0x02, data)
    data(getByteIndex) = s.get.id
  }

  def fromSetPayload(data: Array[Int]): Option[Setting] = {
    if (data(0) != 0x01 || !Setting.checkBit(data(2), bit))
      return None

    val wv = data(setByteIndex) & 0x0f
    val adjust = if ((data(setByteIndex) & 0xF0) == 0x80) true else false
    Some(new WideVaneSetting(wv, adjust))
  }

  def toSetPayload(s: WideVaneSetting, data: Array[Int]): Unit = {
    Setting.validateOrSetType(0x01, data)
    data(2) |= bit
    data(setByteIndex) = s.get.id | (if (s.adjust) 0x80 else 0x00)
  }
}

case class TempSetting(init: Double, initTempMode: Boolean) extends Setting {
  val tempMode = initTempMode
  private var temp = {
    if (initTempMode) {
      (init - 128) / 2.0
    } else {
      31 - init
    }
  }

  def setTemp(t: Double): Unit = if (t >= 16 && t <= 31) temp = t
  def getTemp: Double = temp
  private def getTempForWrite: Double = {
    if (tempMode) {
      temp * 2.0 + 128
    } else {
      31 - temp
    }
  }

  override def toString: String = "Temp: " + temp
}

object TempSetting {
  private val bit = 0x04
  private val getTempModeIndex = 11
  private val getNotTempModeIndex = 5
  private val setTempModeIndex = 14
  private val setNotTempModeIndex = 5

  def fromGetPayload(data: Array[Int]): Option[Setting] = {
    if (data(0) != 0x02)
      return None

    val tempMode = data(getTempModeIndex) != 0
    val v = if (tempMode) data(getTempModeIndex) else data(getNotTempModeIndex)
    Some(new TempSetting(v, tempMode))
  }

  def toGetPayload(s: TempSetting, data: Array[Int]): Unit = {
    Setting.validateOrSetType(0x02, data)
    if (s.tempMode) {
      data(getTempModeIndex) = s.getTempForWrite.toInt
    } else {
      data(getNotTempModeIndex) = s.getTempForWrite.toInt
    }
  }

  def fromSetPayload(data: Array[Int]): Option[Setting] = {
    if (data(0) != 0x01 || !Setting.checkBit(data(1), bit))
      return None

    val tempMode = data(setTempModeIndex) != 0
    val v = if (tempMode) data(setTempModeIndex) else data(setNotTempModeIndex)
    Some(new TempSetting(v, tempMode))

  }

  def toSetPayload(s: TempSetting, data: Array[Int]): Unit = {
    Setting.validateOrSetType(0x01, data)
    data(1) |= bit
    if (s.tempMode) {
      data(setTempModeIndex) = s.getTempForWrite.toInt
    } else {
      data(setNotTempModeIndex) = s.getTempForWrite.toInt
    }
  }
}

case class RoomTempSetting(init: Double, initTempMode: Boolean) extends Setting {
  private val tempMode = initTempMode
  private var temp = {
    if (initTempMode) {
      (init - 128) / 2.0
    } else {
      10 + init
    }
  }

  def setTemp(t: Double): Unit = temp = t
  def getTemp: Double = temp
  def getTempForWrite: Double = {
    if (tempMode) {
      temp * 2.0 + 128
    } else {
      temp - 10
    }
  }

  override def toString: String = "RoomTemp: " + temp
}

object RoomTempSetting {
  private val getTempModeIndex = 6
  private val getNotTempModeIndex = 3

  def fromGetPayload(data: Array[Int]): Option[Setting] = {
    if (data(0) != 0x03)
      return None

    val tempMode = data(getTempModeIndex) != 0
    val v = if (tempMode) data(getTempModeIndex) else data(getNotTempModeIndex)
    Some(new RoomTempSetting(v, tempMode))
  }

  def toGetPayload(s: RoomTempSetting, data: Array[Int]): Unit = {
    Setting.validateOrSetType(0x03, data)
    if (s.tempMode) {
      data(getTempModeIndex) = s.getTempForWrite.toInt
    } else {
      data(getNotTempModeIndex) = s.getTempForWrite.toInt
    }
  }
}

case class RemoteTempSetting(init: Double, initTempMode: Boolean) extends Setting {
  private val tempMode = initTempMode
  private var temp = {
    if (initTempMode) {
      (init - 128) / 2.0
    } else {
      10 + init
    }
  }

  def setTemp(t: Double): Unit = temp = t
  def getTemp: Double = temp
  def getTempForWrite: Double = {
    if (tempMode) {
      temp * 2.0 + 128
    } else {
      temp - 10
    }
  }

  override def toString: String = "RemoteTemp: " + temp
}

object RemoteTempSetting {
  private val getTempModeIndex = 6
  private val getNotTempModeIndex = 3

  def fromGetPayload(data: Array[Int]): Option[Setting] = {
    // shouldn't get called
    if (data(0) != 0x07)
      return None

    val tempMode = data(getTempModeIndex) != 0
    val v = if (tempMode) data(getTempModeIndex) else data(getNotTempModeIndex)
    Some(new RemoteTempSetting(v, tempMode))
  }

  def toGetPayload(s: RemoteTempSetting, data: Array[Int]): Unit = {
    // shouldn't get called
    Setting.validateOrSetType(0x07, data)
    if (s.tempMode) {
      data(getTempModeIndex) = s.getTempForWrite.toInt
    } else {
      data(getNotTempModeIndex) = s.getTempForWrite.toInt
    }
  }

  def fromSetPayload(data: Array[Int]): Option[Setting] = {
    if (data(0) != 0x07 || data(1) != 0x01)
      return None

    Some(new RemoteTempSetting(data(3), true))
  }

  def toSetPayload(s: RemoteTempSetting, data: Array[Int]): Unit = {
    Setting.validateOrSetType(0x07, data)
    data(1) = 0x01
    data(3) = s.getTempForWrite.toInt
  }
}

case class OperatingSetting(var v: Int) extends Setting {
  def set(v: Int): Unit = this.v = v
  def get: Int = v
  override def toString: String = "Operating: " + v
}

object OperatingSetting {
  private val byteIndex = 4

  def fromGetPayload(data: Array[Int]): Option[Setting] = {
    if (data(0) != 0x06)
      return None

    Some(new OperatingSetting(data(byteIndex)))
  }

  def toGetPayload(s: OperatingSetting, data: Array[Int]): Unit = {
    Setting.validateOrSetType(0x06, data)
    data(byteIndex) = s.get
  }
}

case class CompressorFrequencySetting(var v: Int) extends Setting {
  def set(v: Int): Unit = this.v = v
  def get: Int = v
  override def toString: String = "CompressorFrequency: " + v
}

object CompressorFrequencySetting {
  private val byteIndex = 3

  def fromGetPayload(data: Array[Int]): Option[Setting] = {
    if (data(0) != 0x06)
      return None

    Some(new CompressorFrequencySetting(data(byteIndex)))
  }

  def toGetPayload(s: CompressorFrequencySetting, data: Array[Int]): Unit = {
    Setting.validateOrSetType(0x06, data)
    data(byteIndex) = s.get
  }
}

object StandbySettingValues extends Enumeration {
  type Type = Value
  val Off: Type = Value(0x00)
  val Stage1: Type = Value(0x01)
  val Stage2: Type = Value(0x02)
  val Stage3: Type = Value(0x03)
  val Stage4: Type = Value(0x04)
  val Stage5: Type = Value(0x05)
  val Warmup: Type = Value(0x06)
  val Defrost: Type = Value(0x07)
  val Unknown: Type = Value(0x08)
}

case class StandBySetting(var stage: Int, aux: Int, data: IndexedSeq[Int]) extends Setting {
  def set(v: Int): Unit = this.stage = v
  def get: Int = stage
  def convert: StandbySettingValues.Value = {
    if (stage == 0 && aux == 0)
      StandbySettingValues.Off
    else if (stage == 0 && aux == 2)
      StandbySettingValues.Defrost
    else if (aux == 4)
      StandbySettingValues.Warmup
    else if (stage == 1)
      StandbySettingValues.Stage1
    else if (stage == 2)
      StandbySettingValues.Stage2
    else if (stage == 3)
      StandbySettingValues.Stage3
    else if (stage == 4)
      StandbySettingValues.Stage4
    else if (stage == 5)
      StandbySettingValues.Stage5
    else
      StandbySettingValues.Unknown
  }
  override def toString: String = "StandBy: " + convert + " raw: " + stage + " " + data
}

object StandBySetting {
  private val byteIndex = 4

  def fromGetPayload(data: Array[Int]): Option[Setting] = {
    if (data(0) != 0x09)
      return None

    Some(new StandBySetting(data(byteIndex), data(byteIndex - 1), data.toIndexedSeq))
  }

  def toGetPayload(s: StandBySetting, data: Array[Int]): Unit = {
    Setting.validateOrSetType(0x09, data)
  }
}

case class ErrorSetting(var v: Int) extends Setting {
  def set(v: Int): Unit = this.v = v
  def get: Int = v
  def getErrorString: String = {
    v match {
      case 0x80 =>
        "No Error"
      case _ =>
        v.toString
    }
  }
  override def toString: String = "Error: " + getErrorString
}

object ErrorSetting {

  def fromGetPayload(data: Array[Int]): Option[Setting] = {
    if (data(0) != 0x04)
      return None

    Some(new ErrorSetting(data(4)))
  }

  def toGetPayload(s: ErrorSetting, data: Array[Int]): Unit = {
    Setting.validateOrSetType(0x04, data)
    data(4) = s.v
  }
}

case class TimersSetting(var v: Int) extends Setting {
  def set(v: Int): Unit = this.v = v
  def get: Int = v
  override def toString: String = "TimersSetting: " + v
}

object TimersSetting {

  def fromGetPayload(data: Array[Int]): Option[Setting] = {
    if (data(0) != 0x05)
      return None

    Some(new TimersSetting(data(4)))
  }

  def toGetPayload(s: TimersSetting, data: Array[Int]): Unit = {
    Setting.validateOrSetType(0x05, data)
  }
}

case class FunctionSetting(val first: Boolean, val data: Array[Int]) extends Setting {
  def getLogString: String = {
    val map = data.map { v =>
      val code = (v >> 2) & 0xff
      val value = v & 3
      (code + 100) -> value
    }
    "Functions: " + map.mkString(", ")
  }

  override def toString: String = "Functions: " + first + ", " + data.toIndexedSeq.map { x => f"$x%02X" }
}

object FunctionSetting {

  def fromGetPayload(data: Array[Int]): Option[Setting] = {
    if (data(0) != 0x20 && data(0) != 0x22)
      return None

    Some(new FunctionSetting(data(0) == 0x20, data.drop(1)))
  }

  def toGetPayload(s: FunctionSetting, data: Array[Int]): Unit = {
    Setting.validateOrSetType(if (s.first) 0x20 else 0x22, data)
    (0 until 15).foreach { idx =>
      data(idx + 1) = s.data(idx)
    }
  }

  def fromSetPayload(data: Array[Int]): Option[Setting] = {
    if (data(0) != 0x1F && data(0) != 0x21)
      return None

    val first = data(0) == 0x1F
    val functionData = data.drop(1)
    Some(new FunctionSetting(first, functionData))

  }

  def toSetPayload(s: FunctionSetting, data: Array[Int]): Unit = {
    Setting.validateOrSetType(if (s.first) 0x1F else 0x21, data)
    s.data.indices.foreach { idx =>
      data(idx + 1) = s.data(idx)
    }
  }
}


case class PacketHeader(packetType: PacketType.Value, unknown1: Int, unknown2: Int, packetLength: Int) {
  def toBytes: Array[Int] = {
    Array(Packet.MAGIC, packetType.id, unknown1, unknown2, packetLength)
  }
}

trait Packet {
  final def toBytes: Array[Byte] = {
    val headerAndPayLoad = payLoadBytes
    val checksum = (Packet.MAGIC - (headerAndPayLoad.sum & 0xff)) & 0xff
    (headerAndPayLoad ++ Array(checksum)).map { _.toByte }
  }

  protected def payLoadBytes: Array[Int]
}

class BadPacket extends Packet {
  override def payLoadBytes: Array[Int] = ???
}

case class ConnectRequestPacket(unknown1: Int, unknown2: Int) extends Packet {
  private val data = Array(0xca, 0x01)
  override def payLoadBytes: Array[Int] = PacketHeader(PacketType.ConnectRequest, unknown1, unknown2, data.length).toBytes ++ data
}

case class ConnectResponsePacket(unknown1: Int, unknown2: Int) extends Packet {
  private val data = Array(0x00)
  override def payLoadBytes: Array[Int] = PacketHeader(PacketType.ConnectResponse, unknown1, unknown2, data.length).toBytes ++ data
}

case class ExtendedConnectRequestPacket(unknown1: Int, unknown2: Int) extends Packet {
  private val data = Array(0xc9)
  override def payLoadBytes: Array[Int] = PacketHeader(PacketType.ExtendedConnectRequest, unknown1, unknown2, data.length).toBytes ++ data
}

case class ExtendedConnectResponsePacket(unknown1: Int, unknown2: Int) extends Packet {
  // E4 -> function support, 0c no support
  // 05 > 1/2/3/Auto, 75 -> 1/2/3/4/Auto, oscillate support
  //private val data = Array(0xc9, 0x03, 0x00, 0x20, 0x00, 0x0A, 0x07, 0x05, 0xE4, 0x25, 0xa6, 0xbc, 0x94, 0xb8, 0xa6, 0xb8) // air handler
  //private val data = Array(0xc9, 0x03, 0x00, 0x20, 0x00, 0x14, 0x07, 0x75, 0x0c, 0x05, 0xa0, 0xbe, 0x94, 0xbe, 0xa0, 0xbe) // unknown lekbob
  //private val data = Array(0xc9, 0x03, 0x00, 0x20, 0x00, 0x14, 0x07, 0x75, 0x8c, 0x25, 0xa0, 0xbe, 0x94, 0xbe, 0xa0, 0xbe, 0x09) // SwiCago
  private val data = Array(0xc9, 0x03, 0x00, 0x20, 0x00, 0x0A, 0x07, 0x05, 0xE4, 0x25, 0xa6, 0xbc, 0x94, 0xb8, 0xa6, 0xb8) // air handler
  override def payLoadBytes: Array[Int] = PacketHeader(PacketType.ExtendedConnectResponse, unknown1, unknown2, data.length).toBytes ++ data
}


case class SetRequestPacket(
    unknown1: Int,
    unknown2: Int,
    settings: IndexedSeq[Setting]
) extends Packet {
  private val data = new Array[Int](16)

  settings.foreach {
    case s: PowerSetting => PowerSetting.toSetPayload(s, data)
    case s: ModeSetting => ModeSetting.toSetPayload(s, data)
    case s: FanSetting => FanSetting.toSetPayload(s, data)
    case s: TempSetting => TempSetting.toSetPayload(s, data)
    case s: RemoteTempSetting => RemoteTempSetting.toSetPayload(s, data)
    case s: VaneSetting => VaneSetting.toSetPayload(s, data)
    case s: WideVaneSetting => WideVaneSetting.toSetPayload(s, data)
    case s: FunctionSetting => FunctionSetting.toSetPayload(s, data)
  }
  override def payLoadBytes: Array[Int] = PacketHeader(PacketType.SetRequest, unknown1, unknown2, data.length).toBytes ++ data
}

case class SetResponsePacket(
    unknown1: Int,
    unknown2: Int
) extends Packet {
  private val data = new Array[Int](16)
  override def payLoadBytes: Array[Int] = PacketHeader(PacketType.SetResponse, unknown1, unknown2, data.length).toBytes ++ data
}

object GetRequestMode extends Enumeration {
  type Type = Value
  val Unknown: Type = Value(0x00)
  val Settings: Type = Value(0x02)
  val RoomTemp: Type = Value(0x03)
  val Errors: Type = Value(0x04)
  val Timers: Type = Value(0x05)
  val Status: Type = Value(0x06)
  val RemoteTemp: Type = Value(0x07)
  val StandBy: Type = Value(0x09)
  val Functions1: Type = Value(0x20)
  val Functions2: Type = Value(0x22)
}

case class GetRequestPacket(
    unknown1: Int,
    unknown2: Int,
    mode: GetRequestMode.Type
) extends Packet {
  private val data = new Array[Int](16)
  data(0) = mode.id
  override def payLoadBytes: Array[Int] = PacketHeader(PacketType.GetRequest, unknown1, unknown2, data.length).toBytes ++ data
}

case class GetResponsePacket(
    unknown1: Int,
    unknown2: Int,
    settings: IndexedSeq[Setting]
) extends Packet {
  private val data = new Array[Int](16)
  settings.foreach {
    case s: PowerSetting => PowerSetting.toGetPayload(s, data)
    case s: ModeSetting => ModeSetting.toGetPayload(s, data)
    case s: FanSetting => FanSetting.toGetPayload(s, data)
    case s: TempSetting => TempSetting.toGetPayload(s, data)
    case s: VaneSetting => VaneSetting.toGetPayload(s, data)
    case s: WideVaneSetting => WideVaneSetting.toGetPayload(s, data)
    case s: RoomTempSetting => RoomTempSetting.toGetPayload(s, data)
    case s: RemoteTempSetting => RemoteTempSetting.toGetPayload(s, data)
    case s: OperatingSetting => OperatingSetting.toGetPayload(s, data)
    case s: CompressorFrequencySetting => CompressorFrequencySetting.toGetPayload(s, data)
    case s: StandBySetting => StandBySetting.toGetPayload(s, data)
    case s: ErrorSetting => ErrorSetting.toGetPayload(s, data)
    case s: TimersSetting => TimersSetting.toGetPayload(s, data)
    case s: FunctionSetting => FunctionSetting.toGetPayload(s, data)
  }
  override def payLoadBytes: Array[Int] = PacketHeader(PacketType.GetResponse, unknown1, unknown2, data.length).toBytes ++ data
}

class HeatPump(port: CommunicationInterface) {
  var isConnected: Boolean = false
  //var readBuffer = new scala.collection.mutable.ArrayBuffer[Byte]()
  var readBuffer = new Array[Int](1024)
  var readBufferSize = 0
  var closed = false

  def loop(): Unit = {
    while(!closed) {
      readIncomingData()
      if (readBufferSize != 0)
        handleIncomingData()
      Thread.sleep(1)
    }
  }

  @tailrec
  private def handleIncomingData(): Unit = {
    val packet = parsePacket()
    packet match {
      case None =>
        // do nothing
      case Some((packet, packetLength)) =>
        // shift any bytes after the current packet since they are part of the next packet we need to process
        (packetLength until readBufferSize).foreach { i =>
          readBuffer(i - packetLength) = readBuffer(i)
        }
        // set everything else after to 0 to make debugging easier
        ((readBufferSize - packetLength) until readBufferSize).foreach { i =>
          readBuffer(i) = 0
        }
        readBufferSize -= packetLength

        handlePacket(packet)
        handleIncomingData()
    }
  }

  def parsePacket(): Option[(Packet, Int)] = {
    if (readBufferSize < 5)
      return None // no point in checking until we have at least 5 bytes

    var i = 0
    var startIndex = -1
    while (i < readBufferSize && startIndex == -1) {
      if (readBuffer(i) == Packet.MAGIC) {
        startIndex = i
      }

      i += 1
    }

    if (startIndex == -1)
      return Some((new BadPacket(), i))

    val headerSize = 4 // not including MAGIC
    val packetLengthOffset = 4
    if (startIndex  + headerSize + 1 > readBufferSize)
      return None // not enough bytes to read the header

    val packetSize = readBuffer(startIndex + packetLengthOffset)
    val checksumSize = 1
    val packetEnd = startIndex  + 1+headerSize + packetSize + checksumSize
    val MAX_PACKET_SIZE = 5 /*header */+ 16 /* data */+ 1 /* checksum */

    if (packetSize> MAX_PACKET_SIZE)
      return Some((new BadPacket(), startIndex + packetLengthOffset))

    if (packetEnd > readBufferSize)
      return None // packet isn't complete yet

    val rawPacket = new Array[Int](packetEnd - startIndex)
    System.arraycopy(readBuffer, startIndex, rawPacket, 0, rawPacket.length)
    println(java.time.LocalDateTime.now().toString + " Packet received: " + rawPacket.toIndexedSeq.map { x => f"$x%02X" })

    Packet.parsePacket(rawPacket).map { p => (p, packetEnd)}
  }

  var powerSetting: PowerSetting = new PowerSetting(PowerSettingValues.On)
  var modeSetting: ModeSetting = new ModeSetting(ModeSettingValues.Heat)
  var fanSetting: FanSetting = new FanSetting(FanSettingValues.F1)
  var tempSetting: TempSetting = new TempSetting(22.5, true)
  tempSetting.setTemp(22.5)
  var vaneSetting: VaneSetting = new VaneSetting(VaneSettingValues.Swing)
  var wideVaneSetting: WideVaneSetting = new WideVaneSetting(WideVaneSettingValues.Swing, false)
  var remoteTempSetting: RemoteTempSetting = new RemoteTempSetting(16, false)
  val defaultFunctions1: Array[Int] = Array[Int](0x05, 0x09, 0x0D, 0x11, 0x16, 0x18, 0x3D, 0x41, 0x45, 0x48, 0x4C, 0x50, 0x54, 0x58, 0x00)
  val defaultFunctions2: Array[Int] = Array[Int](0x1F, 0x22, 0x24, 0x29, 0x2D, 0x31, 0x36, 0x38, 0x5D, 0x61, 0x65, 0x69, 0x6D, 0x72, 0x00)
  var functionSetting1: FunctionSetting = new FunctionSetting(true, defaultFunctions1)
  var functionSetting2: FunctionSetting = new FunctionSetting(false, defaultFunctions2)
  var compressorFreq: Int = 0

  def handlePacket(packet: Packet): Unit = {
    packet match {
      case _: ConnectRequestPacket =>
        val bytes = new ConnectResponsePacket(0x01, 0x30).toBytes
        Thread.sleep(1)
        println("****** Connection request packet, sending response back: " + bytes.toIndexedSeq.map { x => f"$x%02X" })
        port.sendBytes(bytes)
      case _: ExtendedConnectRequestPacket =>
        val bytes = new ExtendedConnectResponsePacket(0x01, 0x30).toBytes
        println("******++++++ Extended connection request packet, sending response back: " + bytes.toIndexedSeq.map { x => f"$x%02X" })
        port.sendBytes(bytes)
      case srp: SetRequestPacket =>
        powerSetting = srp.settings.find { _.isInstanceOf[PowerSetting ]}.getOrElse(powerSetting).asInstanceOf[PowerSetting]
        modeSetting = srp.settings.find { _.isInstanceOf[ModeSetting ]}.getOrElse(modeSetting).asInstanceOf[ModeSetting]
        fanSetting = srp.settings.find { _.isInstanceOf[FanSetting ]}.getOrElse(fanSetting).asInstanceOf[FanSetting]
        tempSetting = srp.settings.find { _.isInstanceOf[TempSetting ]}.getOrElse(tempSetting).asInstanceOf[TempSetting]
        vaneSetting = srp.settings.find { _.isInstanceOf[VaneSetting ]}.getOrElse(vaneSetting).asInstanceOf[VaneSetting]
        wideVaneSetting = srp.settings.find { _.isInstanceOf[WideVaneSetting ]}.getOrElse(wideVaneSetting).asInstanceOf[WideVaneSetting]
        remoteTempSetting = srp.settings.find { _.isInstanceOf[RemoteTempSetting ]}.getOrElse(remoteTempSetting).asInstanceOf[RemoteTempSetting]
        val functionSetting = srp.settings.find { _.isInstanceOf[FunctionSetting ]}.map { _.asInstanceOf[FunctionSetting] }
        functionSetting1 = functionSetting.find { _.first == true }.getOrElse(functionSetting1)
        functionSetting2 = functionSetting.find { _.first == false }.getOrElse(functionSetting2)
        println(java.time.LocalDateTime.now().toString + s"Set request: $srp")
        if (functionSetting.isDefined) {
          println("Functions1: " + functionSetting1)
          println("Functions2: " + functionSetting2)
        }
        port.sendBytes(new SetResponsePacket(0x01, 0x30).toBytes)
      case grp: GetRequestPacket =>
        val response = grp.mode match {
          case GetRequestMode.Timers =>
            Some(new GetResponsePacket(0x01, 0x30, IndexedSeq(new TimersSetting(0))))

          case GetRequestMode.Errors =>
            Some(new GetResponsePacket(0x01, 0x30, IndexedSeq(new ErrorSetting(0x80))))

          case GetRequestMode.StandBy =>
            Some(new GetResponsePacket(0x01, 0x30, IndexedSeq(new StandBySetting(0, 0, IndexedSeq()))))

          case GetRequestMode.Settings =>
            val settings = IndexedSeq(powerSetting, modeSetting, fanSetting, tempSetting, vaneSetting, wideVaneSetting)
            Some(new GetResponsePacket(0x01, 0x30, settings))

          case GetRequestMode.RoomTemp =>
            val roomTemp = new RoomTempSetting(22.5, true)
            roomTemp.setTemp(remoteTempSetting.getTemp)
            Some(new GetResponsePacket(0x01, 0x30, IndexedSeq(roomTemp)))

          case GetRequestMode.Status =>
            val operating = new OperatingSetting(1)
            val frequency = new CompressorFrequencySetting(compressorFreq)
            compressorFreq = (compressorFreq + 1) % 60
            Some(new GetResponsePacket(0x01, 0x30, IndexedSeq(operating, frequency)))

          case GetRequestMode.RemoteTemp =>
            /*val remoteTemp = new RemoteTempSetting(22.5, true)
            remoteTemp.setTemp(22.5)
            Some(new GetResponsePacket(0x01, 0x30, IndexedSeq(remoteTemp)))*/
          throw new Exception("Not expecting GetRoomTemp call")

          case GetRequestMode.Functions1 =>
            Some(new GetResponsePacket(0x01, 0x30, IndexedSeq(functionSetting1)))

          case GetRequestMode.Functions2 =>
            Some(new GetResponsePacket(0x01, 0x30, IndexedSeq(functionSetting2)))
        }
        response.foreach { r =>
          println(java.time.LocalDateTime.now().toString + s" Get response: ${r}, bytes: " + r.toBytes.toIndexedSeq.map { x => f"$x%02X" })
          port.sendBytes(r.toBytes)
        }
      case _ =>
        println(s"Not handling packet $packet")

    }
  }

  def readIncomingData(): Unit = {
    val bytesToRead = port.bytesAvailable()
    if (bytesToRead < 0) {
      handleClose()
      return
    }

    if (bytesToRead == 0)
      return

    val byteBuffer = new Array[Byte](bytesToRead)
    val bytesRead = port.readBytes(byteBuffer, bytesToRead)
    if (bytesRead != bytesToRead) {
      throw new Exception(s"Didn't read expected number of bytes, was expecting $bytesToRead but read $bytesRead")
    }
    System.arraycopy(byteBuffer.map { b => b & 0xff}, 0, readBuffer, readBufferSize, bytesToRead)
    readBufferSize += bytesToRead
  }

  def handleClose(): Unit = {
    resetReadBuffer()
    closed = true
  }

  def resetReadBuffer(): Unit = {
    readBufferSize = 0
    readBuffer.indices.foreach { i => readBuffer(i) = 0 }
  }
}

object HeatPump {
  def hexCharToInt(c: Char): Int = {
    if (c < '0' || c > 'F') return -1
    if (c > '9' && c < 'A') return -1
    if (c < 'A') c - '0'
    else c - 'A' + 10
  }

  def findSetting[T](clazz: Class[_], settings: IndexedSeq[Setting]): T = {
    val s = settings.find (_.getClass == clazz)
    s.getOrElse(???).asInstanceOf[T]
  }

  def dumpCapture(): Unit = {
    val fileName = "heatpump-capture.bin"
    val inputStream = new DataInputStream(new FileInputStream(fileName))
    var previousTs = 0L
    val prevSettings = Array.fill[Setting](9) {null}

    while (inputStream.available() > 0) {
      val header = inputStream.readInt()
      val size = header & (0x10000 - 1)
      val timestamp = inputStream.readLong()
      previousTs = timestamp
      val data = inputStream.readNBytes(size)

      val sb = new StringBuilder
      for (b <- data) {
        sb.append(String.format("%02X", Byte.box(b)))
        sb.append(" ")
      }
      sb.toString

      val localDateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(timestamp), ZoneId.systemDefault)

      //println(s"$localDateTime $deltaTs Packet $direction: $sb")
      val packet = Packet.parsePacket(data)
      //println(packet)

      def changeAndPrint(i: Int, s: Setting): Unit = {
        if (prevSettings(i) != s) {
          prevSettings(i) = s
          println(s"$localDateTime: " + s)
        }
      }

      if (packet.isDefined) {
        packet.get match {
          case p: SetRequestPacket if p.settings.exists(_.isInstanceOf[RemoteTempSetting]) =>
            val setting = findSetting[RemoteTempSetting](classOf[RemoteTempSetting], p.settings)
            changeAndPrint(0, setting)

          case p: SetRequestPacket if p.settings.exists(_.isInstanceOf[StandBySetting]) =>
            val setting = findSetting[StandBySetting](classOf[StandBySetting], p.settings)
            println("Set standby " + setting)

          case p: GetResponsePacket if p.settings.exists(_.isInstanceOf[OperatingSetting]) =>
            val operating = findSetting[OperatingSetting](classOf[OperatingSetting], p.settings)
            val freq = findSetting[CompressorFrequencySetting](classOf[CompressorFrequencySetting], p.settings)
            changeAndPrint(1, operating)
            changeAndPrint(2, freq)

          case p: GetResponsePacket if p.settings.exists(_.isInstanceOf[PowerSetting]) =>
            val power = findSetting[PowerSetting](classOf[PowerSetting], p.settings)
            val fan = findSetting[FanSetting](classOf[FanSetting], p.settings)
            val mode = findSetting[ModeSetting](classOf[ModeSetting], p.settings)
            val temp = findSetting[TempSetting](classOf[TempSetting], p.settings)
            changeAndPrint(3, power)
            changeAndPrint(4, fan)
            changeAndPrint(5, mode)
            changeAndPrint(6, temp)

          case p: GetResponsePacket if p.settings.exists(_.isInstanceOf[RoomTempSetting]) =>
            val setting = findSetting[RoomTempSetting](classOf[RoomTempSetting], p.settings)
            changeAndPrint(7, setting)

          case p: GetResponsePacket if p.settings.exists(_.isInstanceOf[StandBySetting]) =>
            val setting = findSetting[StandBySetting](classOf[StandBySetting], p.settings)
            changeAndPrint(8, setting)

          case p: GetResponsePacket if p.settings.exists(_.isInstanceOf[FunctionSetting]) =>
            val setting = findSetting[FunctionSetting](classOf[FunctionSetting], p.settings)
            println(setting.getLogString)

          case _ =>

        }
      }
    }

    inputStream.close()

  }

  def test(port: SerialPort): Unit = {
    testConnect(port)
    testConnectWithPreamble(port)
    testTwoConnectsWithPreamble(port)
  }

  def testConnect(port: SerialPort): Unit = {
    val rawPacket = Array(0xfc, 0x5a, 0x01, 0x30, 0x02, 0xca, 0x01, 0xa8)
    val ac = new ArrayCommunication(rawPacket)
    val hp = new HeatPump(ac)
    hp.loop()
    val response = Packet.parsePacket(ac.writtenBytes.toArray)
    println(response)
  }

  def testConnectWithPreamble(port: SerialPort): Unit = {
    val rawPacket = Array(0x5a, 0x01, 0x30, 0x02, 0xca, 0x01, 0xa8, 0xfc, 0x5a, 0x01, 0x30, 0x02, 0xca, 0x01, 0xa8)
    val ac = new ArrayCommunication(rawPacket, 1)
    val hp = new HeatPump(ac)
    hp.loop()
    val response = Packet.parsePacket(ac.writtenBytes.toArray)
    println(response)
  }

  def testTwoConnectsWithPreamble(port: SerialPort): Unit = {
    val rawPacket = Array(0x5a, 0x01, 0x30, 0x02, 0xca, 0x01, 0xa8, 0xfc, 0x5a, 0x01, 0x30, 0x02, 0xca, 0x01, 0xa8, 0xfc, 0x5a, 0x01, 0x30, 0x02, 0xca, 0x01, 0xa8)
    val ac = new ArrayCommunication(rawPacket)
    val hp = new HeatPump(ac)
    hp.loop()
    val response = Packet.parsePackets(ac.writtenBytes.toArray)
    println(response)
  }
}

object SerialHeatPump {
  def main(args: Array[String]): Unit = {
    //HeatPump.dumpCapture()
    val expectedPort = "COM5"
    val ports = SerialPort.getCommPorts.toIndexedSeq
    val serialPort = ports.find { _.getSystemPortName == expectedPort}.getOrElse(throw new Exception(s"Expected port not found $expectedPort"))
    println(serialPort)
    serialPort.setBaudRate(2400)
    serialPort.setComPortTimeouts(SerialPort.TIMEOUT_READ_SEMI_BLOCKING, 0, 0);
    serialPort.setParity(SerialPort.EVEN_PARITY)
    if (!serialPort.openPort()) {
      throw new Exception("Failed to open port")
    }

    //test(serialPort)
    val sc = new SerialCommunication(serialPort)
    val hp = new HeatPump(sc)

    while (true) {
      hp.loop()
    }

    serialPort.closePort()
  }
}



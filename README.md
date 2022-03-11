# Mitsubishi Heat Pump Emulator

This project emulates a Mitsubishi Heat Pump. I've built this mostly by analyzing the packets between MHK1 and my air handler, and also by studying libraries such as https://github.com/SwiCago/HeatPump.

Goal of this project is to make it easier to develop libraries that talk to Mitsubishi heat pumps over the CN105 port. By using this library one program an ESP32/ESP8266 and connect it with this library without having to directly interact with the heat pump. In my case I was developing a library on ESP32 during Winter and I couldn't disconnect my heat pump to play around with it :-)

In order to use you need a USB to TTL Serial adapter with FTDI, or any other means of making a serial connection. 

Find which port is used for the adapter (`COMn` on Windows, or a path to `/dev/ttyUSBn` on Linux), adjust the code to point to the right place:
```
val expectedPort = "COM5"
```

You need to install [sbt](https://www.scala-sbt.org/), then in the root of the project run `sbt run SerialHeatPump.main`.

Note that this project acts as a heat pump, in order to talk to it you need a client such as MHK1, or libraries such as https://github.com/akamali/mhk1_mqtt or https://github.com/SwiCago/HeatPump.

If all is good then you will see
```
[info] running SerialHeatPump
FT232R USB UART
2022-03-10T20:52:26.171769700 Packet received: ArraySeq(FC, 5A, 01, 30, 02, CA, 01, A8)
****** Connection request packet, sending response back: ArraySeq(FC, 7A, 01, 30, 01, 00, 54)
2022-03-10T20:52:27.435174300 Packet received: ArraySeq(FC, 41, 01, 30, 10, 07, 01, 1B, AC, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, AF)
2022-03-10T20:52:27.449824400Set request: SetRequestPacket(1,48,Vector(RemoteTemp: 22.0))
```

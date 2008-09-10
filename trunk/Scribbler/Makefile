# Makefile for creating bytecode from BASIC Stamp 2 file

scribbler_server.bytecode: scribbler_server.bs2
	cd pbasic-linux; ./stampapp ../scribbler_server.bs2 > ../scribbler_server.bytecode

factory_firmware.bytecode: factory_firmware.bs2
	cd pbasic-linux; ./stampapp ../factory_firmware.bs2 > ../factory_firmware.bytecode


clean:
	$(RM) *.bytecode *~

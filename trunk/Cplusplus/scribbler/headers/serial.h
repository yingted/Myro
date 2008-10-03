#ifndef __SERIAL_H__
#define  _SERIAL_H__

/**
 * @class serial
 *
 * @brief Used to manipulate a serial connection in Linux for
 * connecting, reading, and writing. 
 *
 * @Author Richard Edwards
 */
class serial {

	public:

	/**
	 * Constructs a general serial class. However, the only specifiable
	 * parameter for the serial is the baudrate. At a later date,
	 * the class will be update to support a wider range of serial
	 * connections.
	 *
	 * @param baudrate - The communication speed.
	 * @param block_read - specifies whether or not _read should block
	 */
	serial(int baudrate, int block_read);

	~serial();

	/**
	 * Used to connect to a specified device, and configure the
	 * serial channel. The channel is currently only configurable
	 * as 8N1.
	 */
	int connect(const char * device);

	/**
	 * Used to close the serial connection.
	 */
	void disconnect();

	/**
	 * @param buf   - buffer that contains the data to be written.
	 * @param bytes - the number of bytes to write.
	 * @return The number of bytes written.
	 */
	int _write(unsigned char * buf, int bytes);

	/**
	 * Used to read data off of a serial connection, note that if
	 * the serial connection is configured to be non-blocking,
	 * you may not read the desired number of bytes.
	 *
	 * @param buf   - buffer for storing the read data.
	 * @param bytes - the number of bytes to be read. 
	 *
	 * @return The number of bytes read. 
	 */
	int _read(unsigned char * buf, int bytes);

	/**
	 * Flushes the serial input buffer.
	 */
	void flush_input();

	/**
	 * Flushes the serial output buffer.
	 */
	void flush_output();

	private:

	int fd;
	int blocking;
	int baudrate;

};


#endif

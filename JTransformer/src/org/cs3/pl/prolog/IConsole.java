package org.cs3.pl.prolog;

import java.io.IOException;

/* ld:
 * this is one possible solution. might be interesting to have console objects
 * provide java.io streams, but for now this is somewhat simpler.
 */
public interface IConsole {
	public final static int NO_STREAM=0;
	public final static int IN_STREAM=1;
	public final static int OUT_STREAM=2;
	public final static int ERR_STREAM=3;
	public final static int NO_TTY=0;
	public final static int RAW_TTY=1;
	public final static int COOKED_TTY=2;
	/**
	 * write characters to the console.
	 * 
	 * this method should try to write at least 1 and at most buf.length
	 * characters from the buffer to the console. It should block until at least
	 * one character is written.
	 * 
	 * @param stream
	 *                    either OUT_STREAM or ERR_STREAM.
	 * 
	 * @param buf
	 *                    the characters to be written to the console. This argument
	 *                    MUST NOT be null and it MUST NOT be an embty (zero-length)
	 *                    array.
	 * 
	 * @return the number of characters actualy written.
	 * 
	 * @throws IOException
	 *                    if any problems arise during write, that cannot be handled by
	 *                    the console.
	 */
	public int write(int stream, byte[] buf) throws IOException;

	/**
	 * read a character from the console.
	 * 
	 * this method should try to read at least 1 and at most buf.length
	 * characters from the console into the buffer. It should block until at
	 * least one character is read.
	 * 
	 * @param buf
	 *                    the buffer into which the characters should be stored. This
	 *                    argument MUST NOT be null, and it MUST NOT be an emty
	 *                    (zero-length) array.
	 * 
	 * @param ttyMode
	 *                    one of RAW_TTY, COOKED_TTY or NO_TTY. .
	 * 
	 * @return the number of characters actualy read.
	 * 
	 * @throws IOException
	 *                    if any problems arise during write, that cannot be handled by
	 *                    the console.
	 */
	public int read(byte[] buf, int ttyMode) throws IOException;
}
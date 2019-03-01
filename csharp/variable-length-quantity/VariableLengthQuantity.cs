using System;
using System.Collections.Generic;
using System.Linq;

public static class VariableLengthQuantity
{
    public static uint[] Encode(uint[] numbers)
    {
        return numbers.SelectMany(n => EncodeInteger(n), (n, b) => b).ToArray();
    }

    public static uint[] Decode(uint[] bytes)
    {
        int byteIdx = 0;
        List<uint> results = new List<uint>();

        while (byteIdx < bytes.Length)
        {
            results.Add(DecodeBytesFromUint(bytes, ref byteIdx));
        }

        return results.ToArray();

    }

    /// <param name="bytes">the one and only encoded array of bytes</param>
    /// <param name="byteIdx">next position within bytes to be processed </param>
    /// <returns></returns>
    /// <exception cref="InvalidOperationException"></exception>
    private static uint DecodeBytesFromUint(uint[] bytes, ref int byteIdx)
    {
        ulong outBits = 0;
        int bitIdx = 6;
        int outIdx = 63;
        int numBytes = 0;

        while (true)
        {
            if (bitIdx < 0)
            {
                bitIdx = 6;
                if ((bytes[byteIdx] & 0x80) == 0)
                {
                    numBytes++;
                    var result = MakeUintFromBits(outBits, numBytes); 
                    byteIdx++;
                    return result;
                }
                else
                {
                    numBytes++;
                    byteIdx++;
                    if (byteIdx >= bytes.Length)
                        break;
                }
            }

            outBits |= ((bytes[byteIdx] & (1 << bitIdx)) > 0 ? 1ul : 0) << outIdx;
            outIdx--;
            bitIdx--;
        }
        throw new InvalidOperationException();
    }

    /// <param name="bitValue">64 bits with the decoded bits at the far left end</param>
    /// <param name="numBytes">the number of encoded bytes that were used to build bitValue</param>
    /// <returns>a fully decoded integer</returns>
    private static uint MakeUintFromBits(ulong bitValue, int numBytes)
    {
        var byteShift = 4 - numBytes;
        var shift = (byteShift + 4) * 8 + numBytes * 1;

        return Convert.ToUInt32(bitValue >> shift);
    }

    /// <param name="num">an unecoded integer</param>
    /// <returns>an array of 1 to 5 bytes fully encoded and in the correct order</returns>
    private static uint[] EncodeInteger(uint num)
    {
        IList<byte> output = new List<byte>();
        int outByte = 0;
        int outBit = 0;
        long temp = 0;

        void AddByteToOutput()
        {
            if (outByte != 0)
            {
                temp |= 0x80u;
            }
            output.Add(Convert.ToByte(temp));
            temp = 0;
            outByte++;
            outBit = 0;
        }
        for (int ii = 0; ii < 32; ii++)
        {
            if (outBit == 7)
            {
                AddByteToOutput();
            }

            temp |= ((num & (1 << ii)) > 0 ? 1 : 0) << outBit;
            outBit++;
        }
        AddByteToOutput();
        return output.Reverse().SkipWhile(by => by == 0x80u).Select(by => (uint)by).ToArray();
    }
}

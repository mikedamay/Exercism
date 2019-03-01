using System;
using System.Collections.Generic;
using System.Diagnostics;
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
            results.Add(DecodeBytes(bytes, ref byteIdx));
        }

        return results.ToArray();

    }


    private static uint DecodeBytes(uint[] bytes, ref int byteIdx)
    {
        ulong temp = 0;
        int bitIdx = 6;
        int tempIdx = 63;
        int numBytes = 0;

        while (true)
        {
            if (bitIdx < 0)
            {
                bitIdx = 6;
                if ((bytes[byteIdx] & 0x80) == 0)
                {
                    numBytes++;
                    var result = MakeUint(temp, numBytes); 
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

            temp |= ((bytes[byteIdx] & (1 << bitIdx)) > 0 ? 1ul : 0) << tempIdx;
            tempIdx--;
            bitIdx--;
        }
        throw new InvalidOperationException();
    }

    private static uint MakeUint(ulong bitValue, int numBytes)
    {
        var byteShift = 4 - numBytes;
        var shift = (byteShift + 4) * 8 + numBytes * 1;

        return Convert.ToUInt32(bitValue >> shift);
    }


    /// <summary>
    /// 
    /// </summary>
    /// <param name="num">an uncoded integer</param>
    /// <returns>an array of 1 to 5 bytes</returns>
    private static uint[] EncodeInteger(uint num)
    {
        IList<byte> output = new List<byte>();
        int outByte = 0;
        int outBit = 0;
        long temp = 0;

        void AddByte()
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
                AddByte();
            }

            temp |= ((num & (1 << ii)) > 0 ? 1 : 0) << outBit;
            outBit++;
        }
        AddByte();
        return output.Reverse().SkipWhile(by => by == 0x80u).Select(by => (uint)by).ToArray();
    }
}

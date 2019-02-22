using System;
using System.Diagnostics;

public class CircularBuffer<T>
{
    private readonly int capacity;
    private readonly T[] buffer;
    private int lastInIdx;
    private int nextOutIdx;
    private int usage;
    public CircularBuffer(int capacity)
    {
        this.capacity = capacity;
        buffer = new T[capacity];
        Clear();
    }

    public T Read()
    {
        if (usage == 0)
        {
            throw new InvalidOperationException();
        }

        T rtn = buffer[nextOutIdx];
        buffer[nextOutIdx] = default(T);
        Increment(ref nextOutIdx);
        usage--;
        return rtn;
    }

    public void Write(T value)
    {
        if (usage == capacity)
        {
            throw new InvalidOperationException();
        }
        WriteOrOverwrite(value);
        usage++;
    }

    public void Overwrite(T value)
    {
        if (usage < capacity)
        {
            Write(value);
            return;
        }
        WriteOrOverwrite(value);
        Increment(ref nextOutIdx);
    }

    private void WriteOrOverwrite(T value)
    {
        Increment(ref lastInIdx);
        buffer[lastInIdx] = value;
    }

    public void Clear()
    {
        usage = 0;
        lastInIdx = capacity - 1;
        nextOutIdx = 0;
        for (int ii = 0; ii < capacity; ii++)
        {
            buffer[ii] = default(T);
        }
    }

    private void Increment(ref int idx)
    {
        if (++idx == capacity)
        {
            idx = 0;
        }
    }

}
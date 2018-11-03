using System;
using System.Collections;
using System.Collections.Generic;using System.Dynamic;
using System.Linq;

public class SimpleLinkedList<T> : IEnumerable<T>
{
    private class Enumor<T> : IEnumerator<T>
    {
        private bool bof = true;
        private SimpleLinkedList<T> head;
        private SimpleLinkedList<T> node;

        public Enumor(SimpleLinkedList<T> head)
        {
            this.head = head;
            Reset();
        }
        
        public bool MoveNext()
        {
            if (bof)
            {
                bof = false;
                return true;
            }

            if (node.Next != null)
            {
                node = node.Next;
                return true;
            }
            return false;
        }

        public void Reset()
        {
            bof = true;
            node = head;
        }

        public T Current => node.Value;

        object IEnumerator.Current => Current;

        public void Dispose()
        {
        }
    }
    public SimpleLinkedList(T value)
    {
        this.Value = value;
    }
    private SimpleLinkedList(IEnumerator<T> iter)
    {
        Value = iter.Current;
        if (iter.MoveNext())
        {
            Next = new SimpleLinkedList<T>(iter);
        }
    }
    public SimpleLinkedList(IEnumerable<T> values)
    {
/*
        var iter = values.GetEnumerator();
        iter.MoveNext();
        Value = iter.Current;
        if (iter.MoveNext())
            Next = new SimpleLinkedList<T>(iter);
*/
        Value = values.First();
        if (values.Count() > 1)
            Next = new SimpleLinkedList<T>(values.Skip(1));
    }

    public T Value { get; }

    public SimpleLinkedList<T> Next { get; private set; }

    public SimpleLinkedList<T> Add(T value)
    {
        Next = new SimpleLinkedList<T>(value);
        return this;
    }

    public IEnumerator<T> GetEnumerator()
    {
        return new Enumor<T>(this);
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return new Enumor<T>(this);
    }
}
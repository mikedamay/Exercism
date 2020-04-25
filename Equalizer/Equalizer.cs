using System;
using System.Runtime.CompilerServices;
using Microsoft.VisualBasic.CompilerServices;

namespace Equalizer
{
    internal class Basic
    {
        public Basic(int val)
        {
            Val = val;
        }
        public readonly int Val;
    }
    internal class BasicWithOverridenEquals
    {
        public BasicWithOverridenEquals(int val)
        {
            Val = val;
        }
        public readonly int Val;
        
        protected bool Equals(BasicWithOverridenEquals other)
        {
            return Val == other.Val;
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            if (obj.GetType() != this.GetType()) return false;
            return Equals((BasicWithOverridenEquals) obj);
        }
        public override int GetHashCode()
        {
            unchecked
            {
                return Val * 397;
            }
        }        
    }
    internal class BasicWithOverridenOpEq
    {
        public BasicWithOverridenOpEq(int val)
        {
            Val = val;
        }
        public readonly int Val;
        
        protected bool Equals(BasicWithOverridenOpEq other)
        {
            return Val == other.Val;
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            if (obj.GetType() != this.GetType()) return false;
            return Equals((BasicWithOverridenOpEq) obj);
        }
        public override int GetHashCode()
        {
            unchecked
            {
                return Val * 397;
            }
        }

        public static bool operator == (BasicWithOverridenOpEq @this, BasicWithOverridenOpEq other)
            => @this.Equals(other);

        public static bool operator !=(BasicWithOverridenOpEq @this, BasicWithOverridenOpEq other)
            => !(@this == other);
    }
    internal class Executor
    {
        
        
    }
}
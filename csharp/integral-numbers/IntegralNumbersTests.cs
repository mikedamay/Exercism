using System;
using Xunit;

namespace IntegralNumbers
{
    public class TelemetryBufferTests
    {
        [Fact/*(Skip = "Remove this Skip property to run this test")*/]
        public void ToBuffer_upper_long()
        {
            var bytes = TelemetryBuffer.ToBuffer(Int64.MaxValue);
            Assert.Equal(new byte[] {0xf8, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f }, bytes);
        }
        
        [Fact/*(Skip = "Remove this Skip property to run this test")*/]
        public void ToBuffer_lower_long()
        {
            var bytes = TelemetryBuffer.ToBuffer((long)UInt32.MaxValue + 1);
            Assert.Equal(new byte[] {0xf8, 0x0, 0x0, 0x0, 0x0, 0x1, 0x0, 0x0, 0x0 }, bytes);
        }
        
        [Fact/*(Skip = "Remove this Skip property to run this test")*/]
        public void ToBuffer_upper_uint()
        {
            var bytes = TelemetryBuffer.ToBuffer(UInt32.MaxValue);
            Assert.Equal(new byte[] {0x4, 0xff, 0xff, 0xff, 0xff, 0x0, 0x0, 0x0, 0x0 }, bytes);
        }
        
        [Fact/*(Skip = "Remove this Skip property to run this test")*/]
        public void ToBuffer_lower_uint()
        {
            var bytes = TelemetryBuffer.ToBuffer((long)Int32.MaxValue + 1);
            Assert.Equal(new byte[] {0x4, 0x0, 0x0, 0x0, 0x80, 0x0, 0x0, 0x0, 0x0 }, bytes);
        }
        
        [Fact/*(Skip = "Remove this Skip property to run this test")*/]
        public void ToBuffer_upper_int()
        {
            var bytes = TelemetryBuffer.ToBuffer(Int32.MaxValue);
            Assert.Equal(new byte[] {0xfc, 0xff, 0xff, 0xff, 0x7f, 0x0, 0x0, 0x0, 0x0 }, bytes);
        }
        
        [Fact/*(Skip = "Remove this Skip property to run this test")*/]
        public void ToBuffer_lower_int()
        {
            var bytes = TelemetryBuffer.ToBuffer((long)UInt16.MaxValue + 1);
            Assert.Equal(new byte[] {0xfc, 0x0, 0x0, 0x1, 0x0, 0x0, 0x0, 0x0, 0x0 }, bytes);
        }
        
        [Fact/*(Skip = "Remove this Skip property to run this test")*/]
        public void ToBuffer_upper_ushort()
        {
            var bytes = TelemetryBuffer.ToBuffer(UInt16.MaxValue);
            Assert.Equal(new byte[] {0x2, 0xff, 0xff, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 }, bytes);
        }
        
        [Fact/*(Skip = "Remove this Skip property to run this test")*/]
        public void ToBuffer_lower_ushort()
        {
            var bytes = TelemetryBuffer.ToBuffer((long)Int16.MaxValue + 1);
            Assert.Equal(new byte[] {0x2, 0x0, 0x80, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 }, bytes);
        }
        
        [Fact/*(Skip = "Remove this Skip property to run this test")*/]
        public void ToBuffer_upper_short()
        {
            var bytes = TelemetryBuffer.ToBuffer(Int16.MaxValue);
            Assert.Equal(new byte[] {0xfe, 0xff, 0x7f, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 }, bytes);
        }
        
        [Fact]
        public void ToBuffer_Zero()
        {
            var bytes = TelemetryBuffer.ToBuffer(0);
            Assert.Equal(new byte[] {0xfe, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0}, bytes);
        }

        [Fact/*(Skip = "Remove this Skip property to run this test")*/]
        public void ToBuffer_min_short()
        {
            var bytes = TelemetryBuffer.ToBuffer(Int16.MinValue);
            Assert.Equal(new byte[] {0xfe, 0x0, 0x80, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 }, bytes);
        }
        
        [Fact/*(Skip = "Remove this Skip property to run this test")*/]
        public void ToBuffer_min_long()
        {
            var bytes = TelemetryBuffer.ToBuffer(Int64.MinValue);
            Assert.Equal(new byte[] {0xf8, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x80 }, bytes);
        }

        [Fact/*(Skip = "Remove this Skip property to run this test")*/]
        public void ToBuffer_IntMax()
        {
            var bytes = TelemetryBuffer.ToBuffer(Int32.MaxValue);
            Assert.Equal(new byte[] {unchecked((byte)-4), 0xff, 0xff, 0xff, 0x7f, 0, 0, 0, 0 }, bytes);
        }

        [Fact /*(Skip = "Remove this Skip property to run this test")*/]
        public void FromBuffer_IntMax()
        {
            Assert.Equal(Int32.MaxValue, 
                TelemetryBuffer.FromBuffer(new byte[] {unchecked((byte)-4), 0xff, 0xff, 0xff, 0x7f, 0, 0, 0, 0 }));
        }
        
        [Fact /*(Skip = "Remove this Skip property to run this test")*/]
        public void FromBuffer_Invalid()
        {
            Assert.Equal(0, 
                TelemetryBuffer.FromBuffer(new byte[] {22, 0xff, 0xff, 0xff, 0x7f, 0, 0, 0, 0 }));
        }
    }
}
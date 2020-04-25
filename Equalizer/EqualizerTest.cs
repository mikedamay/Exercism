using Xunit;

namespace Equalizer
{
    public class EqualizerTest
    {
        [Fact]
        public void Basic()
        {
            Basic aa = new Basic(42);
            Basic bb = new Basic(42);
            bool bl = aa.Equals(bb);
            Assert.False(bl);
        }
        [Fact]
        public void WithOverridenEquals()
        {
            var aa = new BasicWithOverridenEquals(42);
            var bb = new BasicWithOverridenEquals(42);
            bool bl = aa.Equals(bb);
            Assert.True(bl);
        }
        [Fact]
        public void WithoutOverridenOpEq()
        {
            var aa = new BasicWithOverridenEquals(42);
            var bb = new BasicWithOverridenEquals(42);
            bool bl = aa == bb;
            Assert.False(bl);
        }
        [Fact]
        public void WithOverridenOpEq()
        {
            var aa = new BasicWithOverridenOpEq(42);
            var bb = new BasicWithOverridenOpEq(42);
            bool bl = aa == bb;
            Assert.True(bl);
        }
    }
}
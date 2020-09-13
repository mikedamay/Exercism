using Xunit;
using ExerciseReport;

namespace ExerciseValidation.Tests
{
    public class ExerciseJsonParserTests
    {
        [Fact]
        public void ParseExerciseJson_WithBlankSlug_ReportsError()
        {
            var tcjp = new TrackConfigJsonParser();
            var json = Utils.GetResourceAsString(
                Constants.ExercisesMissingSlugResource);
            var outputs = tcjp.FromString(json); 
            Assert.Equal(Result.Errors, outputs.Result);
        }
    }
}
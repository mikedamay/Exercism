using ExerciseReport.Creation;
using Xunit;

namespace ExerciseReport.Tests
{
    public class ExerciseFileTests
    {
        [Fact]
        public void ReadFile_OnInvalidPath_ReportsError()
        {
            // var efc = new ExerciseFileCollator(new ExerciseFileHandler(PathNames.Test.Root, Constants.CSharpTrack),
            var efc = new ExerciseFileCollator(new ExerciseFileHandler("./", "bad-track"),
                new ExerciseJsonParser());
            var outputs = efc.ReadExercises();
            Assert.Equal(Result.FatalError, outputs.result);
            Assert.NotNull(outputs.errors[0].Message);
        }

        [Fact]
        public void ReadFile_WithInvalidDocType_ReportsError()
        {
            var efc = new ExerciseFileCollator(new ExerciseResourceHandler(), 
                new ExerciseJsonParser());
            efc.ReadExercises();

        }
    }
}
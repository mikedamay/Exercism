using Xunit;

namespace ExerciseReport.Tests
{
    public class ExerciseFileTests
    {
        [Fact]
        public void ReadFile_OnInvalidPath_ReportsFatalError()
        {
            var efc = new ExerciseReader(new ExerciseFileHandler("./", "bad-track"),
                new ExerciseJsonParser());
            var outputs = efc.ReadExercises();
            Assert.Equal(Result.FatalError, outputs.Result);
            Assert.NotNull(outputs.Errors[0].Message);
        }
    }
}
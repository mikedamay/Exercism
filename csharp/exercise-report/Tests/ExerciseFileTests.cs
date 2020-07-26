using System.Text.Json;
using ExerciseReport.Creation;
using Xunit;

namespace ExerciseReport.Tests
{
    public class ExerciseFileTests
    {
        [Fact]
        public void ReadFile_OnInvalidPath_ReportsFatalError()
        {
            // var efc = new ExerciseFileCollator(new ExerciseFileHandler(PathNames.Test.Root, Constants.CSharpTrack),
            var efc = new ExerciseFileCollator(new ExerciseFileHandler("./", "bad-track"),
                new ExerciseJsonParser());
            var outputs = efc.ReadExercises();
            Assert.Equal(Result.FatalError, outputs.result);
            Assert.NotNull(outputs.errors[0].Message);
        }

        [Fact]
        public void ParseExerciseJson_WithInvalidDocType_ReportsFatalError()
        {
            var ejp = new ExerciseJsonParser();
            var json = ExerciseReportTests.GetResourceAsString(
                Constants.ExercisesBadDocTypeResource);
            var results = ejp.FromString(json); 
            Assert.Equal(Result.FatalError, results.result);
            Assert.Contains(nameof(Exercise.DocumentType), results.errors[0].Message);
        }

        [Fact]
        public void ParseExerciseJson_WithInvalidLevel_ReportFatalsError()
        {
            var ejp = new ExerciseJsonParser();
            var json = ExerciseReportTests.GetResourceAsString(
                Constants.ExercisesBadLevelResource);
            var results = ejp.FromString(json); 
            Assert.Equal(Result.FatalError, results.result);
            Assert.Contains(nameof(Exercise.Level), results.errors[0].Message);
        }
        
        [Fact]
        public void ParseJson_WithWrongStructure_ReportsFatalError()
        {
            var ejp = new ExerciseJsonParser();
            var json = ExerciseReportTests.GetResourceAsString(
                Constants.ExercisesWrongStructureResource);
            var results = ejp.FromString(json); 
            Assert.Equal(Result.FatalError, results.result);
            Assert.Contains(nameof(ExerciseObjectTree), results.errors[0].Message);
        }
        
        [Fact]
        public void ReadFile_WithNonJsonSyntax_ReportsFatalError()
        {
            var efc = new ExerciseFileCollator(new ExerciseResourceHandler(
                    Constants.ManyDesignsResource), 
                new ExerciseJsonParser());
            Assert.Equal(Result.FatalError, efc.ReadExercises().result);
            Assert.NotNull(efc.ReadExercises().errors[0].Message);
        }
    }
}
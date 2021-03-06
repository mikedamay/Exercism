using System.IO;
using Xunit;

namespace ExerciseReport.Tests
{
    public class FSBasedTests
    {
        public FSBasedTests()
        {
            Directory.SetCurrentDirectory(Constants.TestUserRoot);
            var exercisesJson = Utils.GetResourceAsString(Constants.ExercisesResource);
            var path = Path.Combine(
                ".",
                PathNames.Default.Languages,
                Constants.CSharpTrack,
                PathNames.Default.ExerciseFile
            );
            File.WriteAllText(path, exercisesJson);
        }

        [Fact(Skip = "this will hit the 'production' file system - so be careful")]
        public void Merge_WellFormedExerciseFile_ShowsNoErrors()
        {
            var merger = ExerciseMerger.CSharpMerger;
            merger.MergeExercisesAndLearningObjectives();
        }

        [Fact(Skip = "This test will overwrite exercises.json")]
        public void CleanExerciseReportFile()
        {
            var reader = ExerciseReader.CSharpExerciseReader;
            var outputs = reader.ReadExercises();
            var efh = new ExerciseFileHandler(PathNames.Default.Root, Constants.CSharpTrack);
            var ejp = new ExerciseJsonParser();
            var json = ejp.ToString(outputs.ExerciseObjectTree);
            efh.WriteFile(json);
        }
    }
}
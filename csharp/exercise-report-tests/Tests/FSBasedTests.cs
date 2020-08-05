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

        [Fact]
        public void Merge_WellFormedExerciseFile_ShowsNoErrors()
        {
            var merger = ExerciseMerger.CSharpMerger;
            merger.MergeInLearningObjectives();
        }
    }
}
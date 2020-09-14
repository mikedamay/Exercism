using Xunit;

namespace ExerciseValidationTests
{
    public class ExerciseComparerTests
    {
        [Fact]
        public void Compare_MissingInTrackConfig_ReportsNotInTrackConfig()
        {
            var comparer = Utils.GetComparerFromResources(
                Constants.ExerciseNotInTrackResource,
                Constants.TrackNotInTrackResource
            );
            var mergeResults = comparer.CompareExercises();
            
        }
    }
}
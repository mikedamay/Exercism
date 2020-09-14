using ExerciseValidation;
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
            var expected = new ExerciseAndConcept[]
            {
                new ExerciseAndConcept("arrays", "for-loops")
            };
            var compareResults = comparer.CompareExercises();
            Assert.Equal(expected, compareResults.NotInTrackConfig);
        }
    }
}
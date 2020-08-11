using System.Collections.Generic;
using Xunit;

namespace ExerciseReport.Tests
{
    public class ExerciseMergerTests
    {
        private static ErrorResourceHandler testErrorResourceHandler
          = new ErrorResourceHandler();
        private static ErrorWriter CSharpTestErrorWriter { get; } =
            new ErrorWriter(
                testErrorResourceHandler, 
                new ErrorJsonParser());

        [Fact]
        public void ParseDesignDoc_WithMultipleHashes_ReportsNoErrors()
        {
            (var merger, var exerciseResourceHandler) = Utils.GetMergerFromResourcesPlusHandler(
                Constants.ExercisesMultipleObjectivesResource,
                Constants.DesignMultipleHashesResource
            );
            WriteMergeResults(merger.MergeExercisesAndLearningObjectives(), exerciseResourceHandler);
            Assert.Contains("\"know how to use string interpolation on values of any type\"",
                exerciseResourceHandler.ExerciseResultJson);
        }

        [Fact]
        public void Merge_LearningObjectivesWithNoMatchingConcepts_ReportsErrors()
        {
            (var merger, var exerciseResourceHandler) = Utils.GetMergerFromResourcesPlusHandler(
                Constants.ExercisesOrphanedConceptsResource,
                Constants.DesignOrphanedConceptsResource
            );
            WriteMergeResults(merger.MergeExercisesAndLearningObjectives(), exerciseResourceHandler);
            Assert.NotEmpty(testErrorResourceHandler.ResultJson);
            Assert.NotEqual("{\n  \"Errors\": []\n}", testErrorResourceHandler.ResultJson);
        }

        [Fact]
        public void Merge_LearningObjectivesWithMatchingConcepts_ReportsNoErrors()
        {
            (var merger, var exerciseResourceHandler) = Utils.GetMergerFromResourcesPlusHandler(
                Constants.ExercisesUnorphanedConceptsResource,
                Constants.DesignUnorphanedConceptsResource
            );
            WriteMergeResults(merger.MergeExercisesAndLearningObjectives(), exerciseResourceHandler);
            Assert.Equal("{\n  \"Errors\": []\n}", testErrorResourceHandler.ResultJson);
        }

        [Fact]
        public void Merge_ConceptsWithNoObjectives_ReportsErrors()
        {
            (var merger, var exerciseResourceHandler) = Utils.GetMergerFromResourcesPlusHandler(
                Constants.ExercisesNoObjectivesResource,
                Constants.DesignEmptyResource
            );
            WriteMergeResults(merger.MergeExercisesAndLearningObjectives(), exerciseResourceHandler);
            Assert.Contains("The string-formatting concept has no learning objectives",
                testErrorResourceHandler.ResultJson);
        }

        [Fact]
        public void Merge_ConceptsWithObjectives_ReportsNoErrors()
        {
            (var merger, var exerciseResourceHandler) = Utils.GetMergerFromResourcesPlusHandler(
                Constants.ExercisesNoObjectivesResource,
                Constants.DesignJustConceptsResource
            );
            WriteMergeResults(merger.MergeExercisesAndLearningObjectives(), exerciseResourceHandler);
            Assert.Equal("{\n  \"Errors\": []\n}", testErrorResourceHandler.ResultJson);
        }

        [Fact]
        public void Merge_WithTooManyErrors_DoesNotWriteExerciseReport()
        {
            var exerciseResourceHandler
                = new ExerciseResourceHandler(Constants.ExercisesNoObjectivesResource);
            var merger =
                new ExerciseMerger(new ExerciseReader(
                        exerciseResourceHandler,
                        new ExerciseJsonParser())
                    , new DesignDocReader(
                        new DesignDocResourceHandler(Constants.DesignEmptyResource),
                        new DesignDocParser()),
                    maxErrors: 1
                );

            WriteMergeResults(merger.MergeExercisesAndLearningObjectives(), exerciseResourceHandler);
            Assert.Empty(exerciseResourceHandler.ExerciseResultJson);
            Assert.Contains("Too many", testErrorResourceHandler.ResultJson);
        }

        private void WriteMergeResults(
            (Result Result, ExerciseObjectTree ExerciseObjectTree, IList<Error> Errors) mergeResults,
            ExerciseResourceHandler exerciseResourceHandler)
        {
            var errorWriter = CSharpTestErrorWriter;
            errorWriter.Write(mergeResults.Errors);
            if (mergeResults.Result != Result.FatalError)
            {
                var exerciseJsonParser = new ExerciseJsonParser();
                var json = exerciseJsonParser.ToString(mergeResults.ExerciseObjectTree);
                exerciseResourceHandler.WriteExerciseFile(json);
            }
        }
    }
}
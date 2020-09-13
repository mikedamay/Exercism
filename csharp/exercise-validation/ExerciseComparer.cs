using System;
using System.Collections.Generic;
using System.Linq;
using ExerciseReport;

namespace ExerciseValidation
{
        public struct ExerciseAndConcept
        {
            public string Exercise { get; }
            public string Concept { get; }

            public ExerciseAndConcept(string exercise, string concept)
            {
                Exercise = exercise;
                Concept = concept;
            }
        }

    public class ExerciseComparer
    {
        private readonly ExerciseReader exerciseReader;
        private readonly TrackConfigReader trackConfigReader;
        private readonly int maxErrors;

        public ExerciseComparer(ExerciseReader exerciseReader,
            TrackConfigReader trackConfigReader,
            int maxErrors = Constants.MaxErrors)
        {
            this.exerciseReader = exerciseReader;
            this.trackConfigReader = trackConfigReader;
            this.maxErrors = maxErrors;
        }

        public static ExerciseComparer CSharpComparer { get; } =
            new ExerciseComparer(
                new ExerciseReader(
                    new ExerciseFileHandler(PathNames.Default.Root, Constants.CSharpTrack),
                    new ExerciseJsonParser()),
                new TrackConfigReader(
                    new TrackConfigFileHandler(PathNames.Default.Root, Constants.CSharpTrack), 
                    new TrackConfigJsonParser())
            );

        public (Result Result,
            IEnumerable<ExerciseAndConcept> NotInExerciseReport,
            IEnumerable<ExerciseAndConcept> NotInTrackConfig,
            List<Error> Errors)
            CompareExercises()
        {
            var exerciseReportResults = exerciseReader.ReadExercises();
            if (exerciseReportResults.Result == Result.FatalError)
            {
                return (exerciseReportResults.Result, 
                    new List<ExerciseAndConcept>(), 
                    new List<ExerciseAndConcept>(),
                    exerciseReportResults.Errors
                    );
            }

            var trackConfigResults = trackConfigReader.ReadExercises();

            var exerciseReport = exerciseReportResults
                .ExerciseObjectTree.Exercises
                .SelectMany(ex => ex.Concepts, (ex, con) => new ExerciseAndConcept(ex.Slug, con.Name));
            var trackConfig = trackConfigResults
                .TrackConfigObjectTree.Exercises.Concept
                .SelectMany(ex => ex.Concepts, (ex, con) => new ExerciseAndConcept(ex.Slug, con));
            var notInTrackConfig = exerciseReport.Except(trackConfig);
            var notInExerciseReport = trackConfig.Except(exerciseReport);
            var combinedErrors = exerciseReportResults.Errors
                .Concat(trackConfigResults.Errors)
                .ToList();
            var maxSeverity = combinedErrors.Select(e => e.Severity).DefaultIfEmpty(Severity.None).Max();
            Result result = SeverityToResult(maxSeverity);
            return (result, notInExerciseReport, notInTrackConfig, combinedErrors);
        }

        private Result SeverityToResult(Severity severity) =>
            severity switch
            {
                Severity.Error => Result.Errors,
                Severity.Fatal => Result.FatalError,
                Severity.None => Result.Success,
                _ => throw new ArgumentException($"unknown error Severity {severity}")
            };
    }
}
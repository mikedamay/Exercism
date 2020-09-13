using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using ExerciseReport;

namespace ExerciseValidation
{
    internal class TrackConfigJsonParser
    {
        private readonly int maxErrors;

        public TrackConfigJsonParser(int maxErrors = -1)
        {
            this.maxErrors = maxErrors;
        }
        public string ToString(TrackConfigObjectTree trackConfigObjectTree)
        {
            var options = new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true,
                WriteIndented = true,
            };
            options.Converters.Add(new JsonStringEnumConverter(JsonNamingPolicy.CamelCase));
            return JsonSerializer.Serialize(trackConfigObjectTree, options);
        }

        public (Result Result, TrackConfigObjectTree, List<Error> Errors)
            FromString(string jsonText)
        {
            var errors = new List<Error>();
            var options = new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true
            };
            try
            {
                var trackConfigObjectTree = JsonSerializer.Deserialize<TrackConfigObjectTree>(jsonText, options);
                if (trackConfigObjectTree.Exercises?.Concept.Count == 0)
                {
                    var message = $"Json parser failed to parse input file starting {jsonText.Substring(0, 20)}";
                    return (
                        Result.FatalError,
                        trackConfigObjectTree,
                        new List<Error> {new Error(ErrorSource.Exercise, Severity.Fatal, message)}
                    );
                }

                if (errors.Count > maxErrors)
                {
                    errors.Add(new Error(ErrorSource.Exercise,
                        Severity.Error,
                        $"Too many errors reading config.json - which should never happen"));
                }
                return (
                    errors.Count == 0
                        ? Result.Success
                        : errors.Count > maxErrors
                            ? Result.FatalError
                            : Result.Errors,
                    trackConfigObjectTree,
                    errors
                );
            }
            catch (JsonException je)
            {
                return (
                    Result.FatalError,
                    new TrackConfigObjectTree(),
                    new List<Error> {new Error(ErrorSource.Exercise, Severity.Fatal, je.Message)}
                );
            }
            catch (Exception e)
            {
                return (
                    Result.FatalError,
                    new TrackConfigObjectTree(),
                    new List<Error> {new Error(ErrorSource.Exercise, Severity.Fatal, "unknown error:" + e.Message)}
                );
            }
        }

        private static List<Error> ValidateExercises(TrackConfigObjectTree trackConfigObjectTree)
        {
            var errors = trackConfigObjectTree.Exercises.Concept.Select(ex => ValidateExercise(ex))
                .Where(exo => !string.IsNullOrWhiteSpace(exo))
                .Select(exo => new Error(ErrorSource.Exercise, Severity.Error, exo))
                .ToList();
            return errors;
        }

        private static string ValidateExercise(Exercise exercise)
        {
            StringBuilder sb = new StringBuilder();
            
            if (string.IsNullOrWhiteSpace(exercise.Slug)) sb.AppendLine("slug: missing for an exercise");
            
            if (string.IsNullOrWhiteSpace(exercise.Uuid)) sb.AppendLine($"uuid: missing for {exercise.Slug}");
            
            if (exercise.Concepts.Count == 0) sb.AppendLine($"concepts: missing for {exercise.Slug}");

            for (int ii = 0; ii < exercise.Concepts.Count; ii++)
            {
                if (string.IsNullOrWhiteSpace(exercise.Concepts[ii]))
                    sb.AppendLine($"blank concept found for {exercise.Slug}");
            }

            return sb.ToString();
        }
    }
}
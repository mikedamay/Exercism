using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using Microsoft.VisualStudio.TestPlatform.ObjectModel;
using Constants = ExerciseReport.Tests.Constants;

namespace ExerciseReport
{
    internal class ExerciseJsonParser
    {
        public string ToString(ExerciseObjectTree exerciseObjectTree)
        {
            var options = new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true,
                WriteIndented = true,
            };
            options.Converters.Add(new JsonStringEnumConverter(JsonNamingPolicy.CamelCase));
            return JsonSerializer.Serialize(exerciseObjectTree, options);
        }

        public (Result result, ExerciseObjectTree, List<Error> errors) 
            FromString(string jsonText)
        {
            var options = new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true
            };
            try
            {
                options.Converters.Add(new JsonStringEnumConverter(JsonNamingPolicy.CamelCase));
                var exerciseObjectTree = JsonSerializer.Deserialize<ExerciseObjectTree>(jsonText, options);
                List<Error> errors = Validate(exerciseObjectTree);
                if (exerciseObjectTree.Exercises.Count == 0)
                {
                    var message = string.IsNullOrWhiteSpace(jsonText)
                        ? "exercise file input was empty"
                        : $"Json parser failed to parse input file starting {jsonText.Substring(0, 20)}";
                    return (
                        Result.FatalError,
                        exerciseObjectTree,
                        new List<Error>{new Error(Severity.Fatal, message)}
                    );
                }
                return (
                    errors.Count == 0 
                        ? Result.Success : errors.Count > Constants.MaxErrors 
                            ? Result.FatalError : Result.Errors,
                    exerciseObjectTree,
                    errors
                );
            }
            catch (JsonException je)
            {
                return (
                    Result.FatalError,
                    new ExerciseObjectTree(),
                    new List<Error> {new Error(Severity.Fatal, je.Message)}
                );
            }
            catch (Exception e)
            {
                return (
                    Result.FatalError,
                    new ExerciseObjectTree(),
                    new List<Error> {new Error(Severity.Fatal, "unknown error:" + e.Message)}
                );
            }
        }

        private List<Error> Validate(ExerciseObjectTree exerciseObjectTree)
        {
            var output = exerciseObjectTree.Exercises.Select(ex => ValidateExercise(ex))
                .Where(exo => !string.IsNullOrWhiteSpace(exo))
                .Select(exo => new Error(Severity.Error, exo))
                .ToList();
            return output;
        }

        private string ValidateExercise(Exercise exercise)
        {
            StringBuilder sb = new StringBuilder();
            if (string.IsNullOrWhiteSpace(exercise.Slug)) sb.AppendLine("slug: missing for an exercise");
            if (exercise.Level == Level.Invalid) sb.AppendLine($"level: missing for {exercise.Slug}");
            if (exercise.DocumentType == DocumentType.Invalid) sb.AppendLine($"document-type: missing for {exercise.Slug}");
            if ((exercise.DocumentType == DocumentType.Design
                 || exercise.DocumentType == DocumentType.Issue)
                && string.IsNullOrWhiteSpace(exercise.DocumentLink)) sb.AppendLine($"document-link: missing for {exercise.Slug}");
            if (exercise.Concepts.Count == 0) sb.AppendLine($"concepts: missing for {exercise.Slug}");
            for (int ii = 0; ii < exercise.Concepts.Count; ii++)
            {
                if (string.IsNullOrWhiteSpace(exercise.Concepts[ii].Name)) sb.AppendLine($"concept.name: missing for {exercise.Slug}");
            }
            return sb.ToString();
        }
    }
}
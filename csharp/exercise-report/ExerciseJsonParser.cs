using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;

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
                    Result.Success,
                    exerciseObjectTree,
                    new List<Error>()
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
    }
}
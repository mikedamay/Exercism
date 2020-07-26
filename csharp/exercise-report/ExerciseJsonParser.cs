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
            FromString(string sampleJson)
        {
            var options = new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true
            };
            try
            {
                options.Converters.Add(new JsonStringEnumConverter(JsonNamingPolicy.CamelCase));
                return (
                    Result.Success,
                    JsonSerializer.Deserialize<ExerciseObjectTree>(sampleJson, options),
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
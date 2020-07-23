using System.Text.Json;
using System.Text.Json.Serialization;

namespace ExerciseReport
{
    public class ExerciseFileJsonHandler
    {
        public string ToString(ExerciseFile exerciseFile)
        {
            var options = new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true,
                WriteIndented = true,
            };
            options.Converters.Add(new JsonStringEnumConverter(JsonNamingPolicy.CamelCase));
            return JsonSerializer.Serialize(exerciseFile, options);
        }

        public ExerciseFile FromString(string sampleJson)
        {
            var options = new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true
            };
            options.Converters.Add(new JsonStringEnumConverter(JsonNamingPolicy.CamelCase));
            return JsonSerializer.Deserialize<ExerciseFile>(sampleJson, options);
        }
    }
}
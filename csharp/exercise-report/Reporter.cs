using System.Linq;
using System.Text;

namespace ExerciseReport
{
    internal class Reporter
    {
        public string CreateReport(ExerciseFile exerciseFile)
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendLine("# C&#35; reference");
            sb.AppendLine();
            sb.AppendLine("## Concepts");
            sb.AppendLine();
            sb.AppendLine(
                "The C# concept exercises are based on concepts. The list below contains the concepts that have been identified for the C# language.");
            sb.AppendLine();
            sb.AppendLine("### Introductory Concepts");
            sb.AppendLine(); 
            GetCncepts(sb, exerciseFile, Level.Introductory);
            return sb.ToString();
        }

        private void GetCncepts(StringBuilder sb, ExerciseFile exerciseFile, Level level)
        {
            var outputs = exerciseFile.Exercises
                .SelectMany(ex => ex.Concepts, (ex,
                    c) => (ex, c))
                .Where(p => p.Item1.Level == level)
                .OrderBy(p => p.Item2.Name)
                .Select(p => FormatOutput(p.Item1, p.Item2));
            foreach (string output in outputs)
            {
                sb.AppendLine(output);
            }
        }

        private string FormatOutput(Exercise exercise, Concept concept)
        {
            return $"- {concept.Name} - exercise: {exercise.Slug}";
        }
    }
}
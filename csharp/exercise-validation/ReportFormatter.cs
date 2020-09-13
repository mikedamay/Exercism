using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net.Sockets;
using System.Text;
using ExerciseValidation;
using static ExerciseReport.Utils;

namespace ExerciseReport
{
    internal class ReportFormatter
    {
        public string CreateReport(IEnumerable<ExerciseAndConcept> notInExerciseReport,
          IEnumerable<ExerciseAndConcept> notInTrackConfig)
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendLine();
            sb.AppendLine("### Concepts in csharp/config.json not in csharp/reference/exercises.json");
            sb.AppendLine(); 
            AddHeadings(sb);
            ReportMissingExercisesAndConcepts(sb, notInExerciseReport);
            sb.AppendLine();
            sb.AppendLine("### Concepts in csharp/reference/exercises.json not in csharp/config.json");
            sb.AppendLine(); 
            AddHeadings(sb);
            ReportMissingExercisesAndConcepts(sb, notInTrackConfig);
            return sb.ToString();
        }

        private void AddHeadings(StringBuilder sb)
        {
            sb.AppendLine("| Exercise | Concept |");
            sb.AppendLine("|----------|---------|");
        }

        private void ReportMissingExercisesAndConcepts(StringBuilder sb,
            IEnumerable<ExerciseAndConcept> notInReport)
        {
            StringBuilder sbNotInReport = new StringBuilder();
            foreach (var exerciseAndConcept in notInReport)
            {
                sb.AppendFormat($"| {exerciseAndConcept.Exercise} | {exerciseAndConcept.Concept} |");
            }

            sb.Append(sb.Length == 0 ? "None" : sbNotInReport.ToString());
        }
    }
}
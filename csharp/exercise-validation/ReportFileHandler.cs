using System.IO;
using ExerciseReport;

namespace ExerciseValidation
{
    public class ReportFileHandler : IReportFileHandler
    {
        private readonly string reportPathAndFileName;

        public ReportFileHandler(string root, string track)
        {
            reportPathAndFileName = Path.Combine(
                root,
                PathNames.Default.Languages,
                track,
                Constants.ExerciseValidationReport);
        }
        
        public void WriteFile(string reportMarkdown)
        {
            File.WriteAllText(reportPathAndFileName, reportMarkdown);
        }
    }
}
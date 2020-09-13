using System.Collections.Generic;
using ExerciseValidation;

namespace ExerciseReport
{
    internal class ReportWriter
    {
        private readonly IReportFileHandler reportFileHandler;
        private readonly ReportFormatter reportFormatter;

        public static ReportWriter CSharpReportWriter { get; } =
            new ReportWriter(new ReportFileHandler(PathNames.Default.Root, Constants.CSharpTrack),
                new ReportFormatter());
        public ReportWriter(IReportFileHandler reportFileHandler,
            ReportFormatter reportFormatter)
        {
            this.reportFileHandler = reportFileHandler;
            this.reportFormatter = reportFormatter;
        }

        public void WriteReport(IEnumerable<ExerciseAndConcept> notInExerciseReport,
            IEnumerable<ExerciseAndConcept> notInTrackConfig)
        {
            string markdown = reportFormatter.CreateReport(notInExerciseReport, notInTrackConfig);
            reportFileHandler.WriteFile(markdown);
        }
    }
}
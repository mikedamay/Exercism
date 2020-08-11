namespace ExerciseReport
{
    internal class ReportReader
    {
        private readonly IReportFileHandler reportFileHandler;
        private readonly ReportFormatter reportFormatter;

        public static ReportReader CSharpReportReader { get; } =
            new ReportReader(new ReportFileHandler(PathNames.Default.Root, Constants.CSharpTrack),
                new ReportFormatter(PathNames.Default.Root));
        public ReportReader(IReportFileHandler reportFileHandler,
            ReportFormatter reportFormatter)
        {
            this.reportFileHandler = reportFileHandler;
            this.reportFormatter = reportFormatter;
        }

        public void WriteReport(ExerciseObjectTree exerciseObjectTree)
        {
            string markdown = reportFormatter.CreateReport(exerciseObjectTree);
            reportFileHandler.WriteFile(markdown);
        }
    }
}
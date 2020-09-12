namespace ExerciseReport
{
    public enum Result 
    {
        Success,
        Errors,
        FatalError
    }

    public enum Severity
    {
        None = 0,
        Error = 1,
        Fatal = 2
    }

    public enum ErrorSource 
    {
        Process,
        Design,
        Exercise,
        Merge,
        None,
        MissingLearningObjective
    }

    public class Error
    {
        public Severity Severity { get; set; } = Severity.None;
        public string Message { get; set; } = string.Empty;
        public ErrorSource Source { get; set; } = ErrorSource.None;

        public Error(ErrorSource source, Severity severity, string message)
        {
            Source = source;
            Severity = severity;
            Message = message;
        }

        public Error()
        {
            
        }
    }
}
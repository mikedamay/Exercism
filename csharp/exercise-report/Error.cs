using Microsoft.VisualStudio.TestPlatform.CommunicationUtilities;

namespace ExerciseReport
{
    internal enum Result 
    {
        Success,
        Errors,
        FatalError
    }

    internal enum Severity
    {
        Error,
        Fatal
    }

    internal class Error
    {
        public Severity Severity { get; }
        public string Message { get; }

        public Error(Severity severity, string message)
        {
            Severity = severity;
            Message = message;
        }
    }
}
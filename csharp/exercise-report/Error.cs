using System.Collections.Generic;
using System.Text.Json.Serialization;

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

    internal enum ErrorSource 
    {
        Process,
        Design,
        Exercise
    }

    internal class Error
    {
        public Severity Severity { get; }
        public string Message { get; }
        public ErrorSource Source { get; }

        public Error(ErrorSource source, Severity severity, string message)
        {
            Source = source;
            Severity = severity;
            Message = message;
        }
    }
}
using System;
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;

namespace ExerciseReport.Tests
{
    internal class DesignDocResoourceHandler : IDesignDocFileHandler
    {
        public IEnumerable<string> GetExerciseDesignsForTrack()
        {
            var resource = ExerciseReportTests.GetResourceAsString(Constants.ManyDesignsResource);
            return Regex.Split(resource, Constants.DesignDocSeparator);
        }
    }
}
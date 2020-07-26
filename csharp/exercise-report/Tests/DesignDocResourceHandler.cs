using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

namespace ExerciseReport.Tests
{
    internal class DesignDocResourceHandler : IDesignDocFileHandler
    {
        public IEnumerable<(string, string)> GetExerciseDesignsForTrack()
        {
            var resource = ExerciseReportTests.GetResourceAsString(Constants.ManyDesignsResource);
            var texts = Regex.Split(resource, Constants.DesignDocSeparator);
            return texts.Select(t => (Constants.ManyDesignsResource, t));
        }
    }
}
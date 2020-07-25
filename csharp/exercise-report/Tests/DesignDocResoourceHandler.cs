using System;
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;

namespace ExerciseReport.Tests
{
    internal class DesignDocResoourceHandler : IDesignDocFileHandler
    {
        public IEnumerable<string> GetExerciseDesignsForTrack(string track)
        {
            Stream? stream = this.GetType().Assembly.GetManifestResourceStream("ExerciseReport.Tests.many_designs.md");
            string resource = string.Empty;
            if (stream != null)
            {
                using (stream)
                using (var reader = new StreamReader(stream))
                    resource = reader.ReadToEnd();
            }

            return Regex.Split(Environment.NewLine + "separator-1729" + Environment.NewLine, resource);
        }
    }
}
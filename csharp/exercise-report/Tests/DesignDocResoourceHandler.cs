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
            Stream? stream = this.GetType().Assembly.GetManifestResourceStream(Constants.ManyDesignsResource);
            string resource = string.Empty;
            if (stream != null)
            {
                using (stream)
                using (var reader = new StreamReader(stream))
                    resource = reader.ReadToEnd();
            }
            else
            {
                throw new NullReferenceException($"{nameof(stream)} is null - missing resource - {Constants.ManyDesignsResource}");
            }

            return Regex.Split(resource, Environment.NewLine + "separator-1729" + Environment.NewLine);
        }
    }
}
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace ExerciseReport.Tests
{
    internal class DesignDocResourceHandler : IDesignDocFileHandler
    {
        private string resourceName;
        public DesignDocResourceHandler(string resourceName = Constants.ManyDesignsResource)
        {
            this.resourceName = resourceName;
        }

        public IEnumerable<(string, string)> GetExerciseDesignsForTrack()
        {
            var resource = Utils.GetResourceAsString(resourceName);
            var texts = Regex.Split(resource, Constants.DesignDocSeparator);
            return texts.Select(t => (resourceName, t));
        }
    }
}
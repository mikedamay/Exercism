using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace ExerciseReport.Tests
{
    internal class DesignDocResourceHandler : IDesignDocFileHandler
    {
        public IEnumerable<(string, string)> GetExerciseDesignsForTrack()
        {
            var resource = Utils.GetResourceAsString(Constants.ManyDesignsResource);
            var texts = Regex.Split(resource, Constants.DesignDocSeparator);
            return texts.Select(t => (Constants.ManyDesignsResource, t));
        }
    }
}
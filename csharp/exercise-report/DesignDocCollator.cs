using System.Collections.Generic;
using System.Linq;

namespace ExerciseReport
{
    internal class DesignDocCollator
    {
        private readonly DesignDocParser designDocParser;
        private readonly IDesignDocFileHandler designDocFileHandler;

        public DesignDocCollator(IDesignDocFileHandler designDocFileHandler, DesignDocParser designDocParser)
        {
            this.designDocParser = designDocParser;
            this.designDocFileHandler = designDocFileHandler;
        }

        public (LearningObjectives learningObjectives, List<Error> errors) 
            GetLearningObjectives(string track)
        {
            var errors = new List<Error>();
            var learningObjectives = new LearningObjectives();
            var conceptsAndObjectives = designDocFileHandler.GetExerciseDesignsForTrack()
                .SelectMany(d => designDocParser.ParseDesignDoc(d.Item1, d.Item2));
            foreach (var conceptAndObjective in conceptsAndObjectives)
            {
                switch (conceptAndObjective)
                {
                    case (true, _, string concept, string objective):
                        learningObjectives.Builder.Add(concept, objective);
                        break;
                    case (false, string error, _, _):
                        errors.Add(new Error(ErrorSource.Design, Severity.Error, error));
                        break;
                }
            }

            return (learningObjectives, errors);
        }
    }
}
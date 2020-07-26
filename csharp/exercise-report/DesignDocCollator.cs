using System.Collections.Generic;
using System.Linq;

namespace ExerciseReport
{
    internal class DesignDocCollator
    {
        private readonly string root;
        private readonly DesignDocParser designDocParser;
        private readonly IDesignDocFileHandler designDocFileHandler;

        public DesignDocCollator(string root, DesignDocParser designDocParser,
            IDesignDocFileHandler designDocFileHandler)
        {
            this.root = root;
            this.designDocParser = designDocParser;
            this.designDocFileHandler = designDocFileHandler;
        }

        public (LearningObjectives learningObjectives, List<string> errors) 
            GetLearningObjectives(string track)
        {
            var errors = new List<string>();
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
                        errors.Add(error);
                        break;
                }
            }

            return (learningObjectives, errors);
        }
    }
}
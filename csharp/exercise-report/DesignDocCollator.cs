using System;
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
            GetAllLearningObjectivesForTrack(string track)
        {
            var errors = new List<Error>();
            var learningObjectives = new LearningObjectives();
            var conceptsAndObjectives = designDocFileHandler.GetExerciseDesignsForTrack()
                .SelectMany(e_and_c => designDocParser.ParseDesignDoc(e_and_c.ExerciseName, e_and_c.ConceptName));
            foreach (var conceptAndObjective in conceptsAndObjectives)
            {
                switch (conceptAndObjective)
                {
                    case (Result.Success, _, string concept, string objective):
                        learningObjectives.Builder.Add(concept, objective);
                        break;
                    case (Result.Errors, string error, _, _):
                        errors.Add(new Error(ErrorSource.Design, Severity.Error, error));
                        break;
                    case (_, _, _, _):
                        throw new ArgumentException();
                }
            }

            return (learningObjectives, errors);
        }
    }
}
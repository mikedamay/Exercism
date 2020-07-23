using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace ExerciseReport
{
    internal class LearningObjectives
    {
        public IBuilder Builder { get; }

        private Dictionary<string, List<string>> concepts = new Dictionary<string, List<string>>();

        public interface IBuilder
        {
            void Add(string conceptName, string learningObjective);
        }

        private class BuilderImpl : IBuilder
        {
            private readonly LearningObjectives _this;

            public BuilderImpl(LearningObjectives _this)
            {
                this._this = _this;
            }

            public void Add(string conceptName, string learningObjective)
            {
                if (!_this.concepts.ContainsKey(conceptName))
                {
                    _this.concepts[conceptName] = new List<string>();
                }

                _this.concepts[conceptName].Add(learningObjective);
            }
        }

        public LearningObjectives()
        {
            Builder = new BuilderImpl(this);
        }

        public IEnumerable<string>? GetList(string conceptName)
        {
            if (!concepts.ContainsKey(conceptName))
            {
                return null;
            }

            return new ReadOnlyCollection<string>(concepts[conceptName]);
        }
    }
}
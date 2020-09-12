using System.IO;
using ExerciseReport;

namespace ExerciseValidation
{
    public interface ITrackConfigFileHandler
    {
        string ReadFile();
    }

    internal class TrackConfigFileHandler : ITrackConfigFileHandler
    {
        private readonly string exercisePathAndFileName;

        public TrackConfigFileHandler(string root, string track)
        {
            exercisePathAndFileName = Path.Combine(
                root,
                PathNames.Default.Languages,
                track,
                PathNames.Default.ExerciseFile);
        }

        public string ReadFile()
        {
            return File.ReadAllText(exercisePathAndFileName);
        }

        public void WriteFile(string exerciseJson)
        {
            File.WriteAllText(exercisePathAndFileName, exerciseJson);
        }

    }
}
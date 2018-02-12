## Dependencies
- ASound: `sudo apt-get install asound libasound2 libasound2-dev`
- Timidity: `sudo apt-get install timidity`
- Lilypond: `sudo apt-get install lilypond ghostscript`

## To create scores
- Export midi with Euterpea's `writeMidi "test.midi" musicProg`
- Convert to lilypond file: `midi2ly test.midi`
- Render postscript file: `lilypond --ps test-midi.ly`
- Convert to pdf: `ps2pdf test-midi.ps`

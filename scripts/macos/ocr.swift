import Foundation
import Vision
import AppKit

func die(_ msg: String) -> Never {
    fputs("\(msg)\n", stderr)
    exit(1)
}

// Get image from clipboard
guard let img = NSPasteboard.general.readObjects(forClasses: [NSImage.self], options: nil)?.first as? NSImage else {
    die("No image found in clipboard.")
}

guard let tiff = img.tiffRepresentation,
      let ciImage = CIImage(data: tiff) else {
    die("Failed to read image from clipboard.")
}

let request = VNRecognizeTextRequest()
request.recognitionLevel = .accurate
request.usesLanguageCorrection = true
request.automaticallyDetectsLanguage = true

let handler = VNImageRequestHandler(ciImage: ciImage, options: [:])

do {
    try handler.perform([request])
    guard let observations = request.results else {
        die("No text recognized.")
    }

    var result = ""
    for observation in observations {
        if let candidate = observation.topCandidates(1).first {
            result += candidate.string + "\n"
        }
    }

    NSPasteboard.general.clearContents()
    NSPasteboard.general.setString(result, forType: .string)

    print("OCR result copied to clipboard ✅")
} catch {
    die("Text recognition failed: \(error.localizedDescription)")
}

#!/usr/bin/env python3
"""
GitHub Copilot CLI Translation Wrapper
Uses GitHub Copilot via the austenstone/copilot-cli action to translate QMD files
"""

import os
import sys
import json
from pathlib import Path


def get_translation_pair(file_path: str) -> tuple:
    """Get the translation pair filename and target language"""
    path = Path(file_path)
    
    if path.stem.endswith('.zh'):
        # Chinese file -> English file
        new_stem = path.stem[:-3]
        return str(path.parent / f"{new_stem}.qmd"), 'en'
    else:
        # English file -> Chinese file
        return str(path.parent / f"{path.stem}.zh.qmd"), 'zh'


def create_translation_prompt(content: str, source_lang: str, target_lang: str) -> str:
    """Create a prompt for GitHub Copilot to translate the content"""
    lang_names = {'en': 'English', 'zh': 'Chinese'}
    
    prompt = f"""You are a professional translator specializing in biomedical and bioinformatics documentation.

Task: Translate the following Quarto (QMD) file from {lang_names[source_lang]} to {lang_names[target_lang]}.

Critical Requirements:
1. **Preserve YAML frontmatter**: Keep all YAML content between --- markers exactly as is, only translate the title field
2. **Do NOT translate code blocks**: Keep all content between ``` markers in original language, including comments
3. **Maintain markdown formatting**: Headers (#), lists (-, *), links ([]()), emphasis (**, *), etc.
4. **Translate text content**: Paragraphs, headings, descriptions
5. **Technical terminology**: Use standard biomedical/bioinformatics terms
6. **Keep URLs and paths unchanged**
7. **Output only the translated content**: No explanations, just the complete translated file

Input file:
{content}

Translated output:"""
    
    return prompt


def generate_copilot_translation_file(input_file: str, output_file: str) -> str:
    """Generate a file that will be used by the Copilot CLI action"""
    try:
        # Read input file
        with open(input_file, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Detect language and determine target
        target_file, target_lang = get_translation_pair(input_file)
        source_lang = 'zh' if target_lang == 'en' else 'en'
        
        # Create prompt
        prompt = create_translation_prompt(content, source_lang, target_lang)
        
        # Create a temp file with instructions for Copilot
        prompt_file = f"{input_file}.copilot_prompt"
        with open(prompt_file, 'w', encoding='utf-8') as f:
            f.write(prompt)
        
        # Output the paths for the workflow to use
        result = {
            "input_file": input_file,
            "output_file": output_file,
            "prompt_file": prompt_file,
            "source_lang": source_lang,
            "target_lang": target_lang
        }
        
        print(json.dumps(result))
        return prompt_file
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


def main():
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input_file>")
        sys.exit(1)
    
    input_file = sys.argv[1]
    
    if not os.path.exists(input_file):
        print(f"Error: File not found: {input_file}", file=sys.stderr)
        sys.exit(1)
    
    output_file, _ = get_translation_pair(input_file)
    generate_copilot_translation_file(input_file, output_file)


if __name__ == '__main__':
    main()

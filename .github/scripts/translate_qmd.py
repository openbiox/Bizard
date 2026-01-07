#!/usr/bin/env python3
"""
Automatic QMD Translation Script for Bizard
Translates English <-> Chinese QMD files using OpenAI API (GitHub Copilot compatible)
"""

import os
import sys
import re
import json
from pathlib import Path
from typing import Tuple, List, Optional
import argparse

try:
    import openai
except ImportError:
    print("Error: openai package not installed. Install with: pip install openai")
    sys.exit(1)


# Constants
LANGUAGE_DETECTION_THRESHOLD = 0.3  # Chinese chars must be > 30% of English to be classified as Chinese
MAX_SPELL_CHECK_CHARS = 8000  # Maximum characters to send for spell checking
TRUNCATION_THRESHOLD = 0.8  # Minimum proportion of content to keep when truncating at word boundary
DEFAULT_MODEL = "gpt-4o-mini"  # Default model for cost efficiency


class QMDTranslator:
    """Handles translation of QMD files between English and Chinese"""
    
    def __init__(self, api_key: str, model: str = DEFAULT_MODEL, base_url: Optional[str] = None):
        """
        Initialize translator with API credentials
        
        Args:
            api_key: API key for the translation service
            model: Model to use for translation (default: gpt-4o-mini)
            base_url: Base URL for API endpoint (optional, for alternative providers like Xiaomi MiMo)
        """
        # Initialize OpenAI client with optional base_url for alternative providers
        if base_url:
            self.client = openai.OpenAI(api_key=api_key, base_url=base_url)
        else:
            self.client = openai.OpenAI(api_key=api_key)
        self.model = model
    
    def detect_language(self, content: str) -> str:
        """
        Detect if content is primarily English or Chinese
        
        Args:
            content: Text content to analyze
            
        Returns:
            'en' for English, 'zh' for Chinese
        """
        # Count Chinese characters
        chinese_chars = len(re.findall(r'[\u4e00-\u9fff]', content))
        # Count English letters
        english_chars = len(re.findall(r'[a-zA-Z]', content))
        
        # Determine language based on character count
        if chinese_chars > english_chars * LANGUAGE_DETECTION_THRESHOLD:
            return 'zh'
        return 'en'
    
    def extract_yaml_frontmatter(self, content: str) -> Tuple[str, str]:
        """
        Extract YAML frontmatter from QMD content
        
        Args:
            content: Full QMD file content
            
        Returns:
            Tuple of (frontmatter, body)
        """
        yaml_pattern = r'^---\s*\n(.*?)\n---\s*\n'
        match = re.match(yaml_pattern, content, re.DOTALL)
        
        if match:
            frontmatter = f"---\n{match.group(1)}\n---\n"
            body = content[match.end():]
            return frontmatter, body
        
        return "", content
    
    def extract_code_blocks(self, content: str) -> Tuple[str, List[str]]:
        """
        Extract code blocks and replace with placeholders
        
        Args:
            content: QMD body content
            
        Returns:
            Tuple of (content with placeholders, list of code blocks)
        """
        code_blocks = []
        placeholder_template = "<<<CODE_BLOCK_{}>>>"
        
        # Match code blocks with language specifier (e.g., ```{r}, ```python)
        # Handles curly braces and various language identifiers
        code_pattern = r'```+[^\n]*\n.*?\n```+'
        
        def replace_code(match):
            code_blocks.append(match.group(0))
            return placeholder_template.format(len(code_blocks) - 1)
        
        processed_content = re.sub(code_pattern, replace_code, content, flags=re.DOTALL)
        return processed_content, code_blocks
    
    def restore_code_blocks(self, content: str, code_blocks: List[str]) -> str:
        """
        Restore code blocks from placeholders
        
        Args:
            content: Content with placeholders
            code_blocks: List of original code blocks
            
        Returns:
            Content with restored code blocks
        """
        for i, block in enumerate(code_blocks):
            placeholder = f"<<<CODE_BLOCK_{i}>>>"
            content = content.replace(placeholder, block)
        return content
    
    def translate_text(self, text: str, source_lang: str, target_lang: str) -> str:
        """
        Translate text using OpenAI API
        
        Args:
            text: Text to translate
            source_lang: Source language ('en' or 'zh')
            target_lang: Target language ('en' or 'zh')
            
        Returns:
            Translated text
        """
        lang_names = {'en': 'English', 'zh': 'Chinese'}
        
        system_prompt = f"""You are a professional translator specializing in biomedical and bioinformatics content.
Translate the following text from {lang_names[source_lang]} to {lang_names[target_lang]}.

Requirements:

**Formatting Preservation:**
1. Preserve all markdown formatting EXACTLY as it appears in the source (headers, lists, links, emphasis, etc.)
2. Do NOT add or remove any markdown syntax
3. Do NOT translate code placeholders like <<<CODE_BLOCK_N>>>

**Content Accuracy:**
4. Maintain the original meaning and technical accuracy
5. Maintain scientific terminology accuracy
6. For technical terms, use commonly accepted translations in the biomedical field
7. Keep the same tone and style
8. Preserve any special characters and symbols

**Output Restrictions:**
9. ONLY translate the text content, do not add explanations or extra content
10. Only return the translated text, no explanations or additions."""

        try:
            response = self.client.chat.completions.create(
                model=self.model,
                messages=[
                    {"role": "system", "content": system_prompt},
                    {"role": "user", "content": text}
                ],
                temperature=0.3,
                max_tokens=4000
            )
            
            return response.choices[0].message.content.strip()
        
        except Exception as e:
            print(f"Error during translation: {e}")
            raise
    
    def translate_yaml_fields(self, yaml_content: str, source_lang: str, target_lang: str) -> str:
        """
        Translate specific fields in YAML frontmatter
        
        Args:
            yaml_content: YAML frontmatter content
            source_lang: Source language
            target_lang: Target language
            
        Returns:
            Translated YAML content
        """
        # Extract title and translate
        title_match = re.search(r'^title:\s*(.+)$', yaml_content, re.MULTILINE)
        if title_match:
            title = title_match.group(1).strip().strip('"\'')
            # Strip any markdown heading symbols from the title
            title = re.sub(r'^#+\s*', '', title).strip()
            translated_title = self.translate_text(title, source_lang, target_lang)
            # Strip markdown heading symbols from translated title as well
            translated_title = re.sub(r'^#+\s*', '', translated_title).strip()
            # Quote the title if it contains YAML special characters
            yaml_special_chars = [':', '#', '[', ']', '{', '}', '|', '>', '&', '*', '!', '@', '`']
            needs_quoting = (
                any(char in translated_title for char in yaml_special_chars) or
                translated_title.startswith('"') or
                translated_title.startswith("'") or
                translated_title != translated_title.strip()  # Leading/trailing spaces
            )
            if needs_quoting and not (translated_title.startswith('"') and translated_title.endswith('"')):
                # Escape any double quotes in the title and wrap in quotes
                translated_title = translated_title.replace('"', '\\"')
                translated_title = f'"{translated_title}"'
            yaml_content = re.sub(
                r'^title:\s*.+$',
                f'title: {translated_title}',
                yaml_content,
                count=1,
                flags=re.MULTILINE
            )
        
        return yaml_content
    
    def translate_qmd_file(self, input_path: str, output_path: str, target_lang: Optional[str] = None) -> bool:
        """
        Translate a QMD file
        
        Args:
            input_path: Path to input QMD file
            output_path: Path to output translated file
            target_lang: Target language ('en' or 'zh'). If None, auto-detect and translate to opposite
            
        Returns:
            True if successful, False otherwise
        """
        try:
            # Read input file
            with open(input_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # Extract YAML frontmatter
            yaml_part, body_part = self.extract_yaml_frontmatter(content)
            
            # Determine source and target languages using filename-based detection
            # Priority: filename-based > target_lang parameter > content-based detection
            input_filename = os.path.basename(input_path)
            output_filename = os.path.basename(output_path)
            
            # Infer language from filenames
            # This system only supports English (.qmd) and Chinese (.zh.qmd) files
            filename_source_lang = None
            filename_target_lang = None
            
            # Determine source language from input filename
            # Only recognize .zh.qmd (Chinese) and plain .qmd (English)
            if input_filename.endswith('.zh.qmd'):
                filename_source_lang = 'zh'
            elif input_filename.endswith('.qmd') and not input_filename.endswith('.zh.qmd'):
                # Ensure it's a plain .qmd file (not .fr.qmd, .de.qmd, etc.)
                # Extract the extension before .qmd to check for language codes
                name_without_qmd = input_filename[:-4]  # Remove .qmd
                if '.' in name_without_qmd:
                    possible_lang = name_without_qmd.split('.')[-1]
                    # If there's a period followed by 2-3 letters, it might be another language
                    if len(possible_lang) in [2, 3] and possible_lang.isalpha() and possible_lang != 'zh':
                        # This looks like a different language extension (e.g., .fr.qmd, .de.qmd)
                        # Don't assume it's English, let content detection handle it
                        filename_source_lang = None
                    else:
                        # No language extension, so it's English
                        filename_source_lang = 'en'
                else:
                    # No additional extension, so it's English
                    filename_source_lang = 'en'
            
            # Determine target language from output filename
            if output_filename.endswith('.zh.qmd'):
                filename_target_lang = 'zh'
            elif output_filename.endswith('.qmd') and not output_filename.endswith('.zh.qmd'):
                # Same check for output filename
                name_without_qmd = output_filename[:-4]
                if '.' in name_without_qmd:
                    possible_lang = name_without_qmd.split('.')[-1]
                    if len(possible_lang) in [2, 3] and possible_lang.isalpha() and possible_lang != 'zh':
                        filename_target_lang = None
                    else:
                        filename_target_lang = 'en'
                else:
                    filename_target_lang = 'en'
            
            # Content-based language detection (for validation)
            content_detected_lang = self.detect_language(body_part)
            
            # Determine actual source language
            if filename_source_lang:
                source_lang = filename_source_lang
                # Validate against content detection
                if content_detected_lang != filename_source_lang:
                    print(f"WARNING: Filename indicates {filename_source_lang} but content appears to be {content_detected_lang}")
                    print(f"         Using filename-based detection: {filename_source_lang}")
            else:
                # Fallback to content detection if filename doesn't indicate language
                source_lang = content_detected_lang
                print(f"INFO: No language indicator in filename, using content detection: {source_lang}")
            
            # Determine target language
            if target_lang is not None:
                # Explicit target language provided via parameter (manual override)
                pass
            elif filename_target_lang:
                # Use filename-based target language
                target_lang = filename_target_lang
            else:
                # Default: translate to opposite language
                target_lang = 'zh' if source_lang == 'en' else 'en'
            
            # Log translation direction clearly
            print(f"[INPUT]  {input_filename} (detected as {source_lang})")
            print(f"[OUTPUT] {output_filename} (translating to {target_lang})")
            print(f"[DIRECTION] {source_lang} -> {target_lang}")
            
            # Extract code blocks
            text_to_translate, code_blocks = self.extract_code_blocks(body_part)
            
            # Translate the text content
            translated_text = self.translate_text(text_to_translate, source_lang, target_lang)
            
            # Restore code blocks
            translated_body = self.restore_code_blocks(translated_text, code_blocks)
            
            # Translate YAML frontmatter
            if yaml_part:
                yaml_content = yaml_part.strip('---\n').strip()
                translated_yaml_content = self.translate_yaml_fields(yaml_content, source_lang, target_lang)
                translated_yaml = f"---\n{translated_yaml_content}\n---\n\n"
            else:
                translated_yaml = ""
            
            # Combine and write output
            final_content = translated_yaml + translated_body
            
            os.makedirs(os.path.dirname(output_path) or '.', exist_ok=True)
            with open(output_path, 'w', encoding='utf-8') as f:
                f.write(final_content)
            
            print(f"✓ Translation complete: {output_path}")
            return True
            
        except Exception as e:
            print(f"✗ Error translating {input_path}: {e}")
            return False
    
    def check_spelling(self, file_path: str) -> List[str]:
        """
        Check for potential spelling errors and typos
        
        Args:
            file_path: Path to QMD file
            
        Returns:
            List of potential issues found
        """
        issues = []
        
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # Use AI to check for spelling errors
            system_prompt = """You are a proofreader for biomedical and bioinformatics documentation.
Check the following text for:
1. Spelling errors
2. Grammar issues
3. Inconsistent terminology
4. Typos

Return a JSON array of issues found, with each issue having:
- line: approximate line number or section
- type: "spelling", "grammar", "terminology", or "typo"
- issue: description of the problem
- suggestion: suggested correction

If no issues found, return an empty array []."""

            # Truncate content at word boundary to avoid cutting mid-word
            truncated_content = content[:MAX_SPELL_CHECK_CHARS]
            if len(content) > MAX_SPELL_CHECK_CHARS:
                # Find last space to avoid cutting words
                last_space = truncated_content.rfind(' ')
                if last_space > MAX_SPELL_CHECK_CHARS * TRUNCATION_THRESHOLD:
                    truncated_content = truncated_content[:last_space]

            response = self.client.chat.completions.create(
                model=self.model,
                messages=[
                    {"role": "system", "content": system_prompt},
                    {"role": "user", "content": truncated_content}
                ],
                temperature=0.1,
                max_tokens=1000
            )
            
            result = response.choices[0].message.content.strip()
            
            # Try to parse as JSON
            try:
                issues_list = json.loads(result)
                if isinstance(issues_list, list):
                    issues = [f"{item.get('type', 'issue')}: {item.get('issue', '')} - Suggestion: {item.get('suggestion', '')}" 
                             for item in issues_list]
            except json.JSONDecodeError:
                # If not valid JSON, return the raw response
                if result and result != "[]":
                    issues = [result]
            
        except Exception as e:
            print(f"Warning: Could not check spelling for {file_path}: {e}")
        
        return issues


def get_translation_pair(file_path: str) -> str:
    """
    Get the translation pair filename for a given QMD file
    
    Args:
        file_path: Path to QMD file
        
    Returns:
        Path to corresponding translation file
    """
    path = Path(file_path)
    
    if path.stem.endswith('.zh'):
        # Chinese file -> English file
        new_stem = path.stem[:-3]  # Remove .zh
        return str(path.parent / f"{new_stem}.qmd")
    else:
        # English file -> Chinese file
        return str(path.parent / f"{path.stem}.zh.qmd")


def main():
    parser = argparse.ArgumentParser(description='Translate QMD files between English and Chinese')
    parser.add_argument('input_files', nargs='+', help='Input QMD file(s) to translate')
    parser.add_argument('--api-key', help='API key (or set AI_Model_API_KEY/OPENAI_API_KEY env var)')
    parser.add_argument('--base-url', help='API base URL for alternative providers (or set AI_Model_BASE_URL env var)')
    parser.add_argument('--model', help=f'Model to use (or set AI_Model_Name env var, default: {DEFAULT_MODEL})')
    parser.add_argument('--target-lang', choices=['en', 'zh'], help='Target language (auto-detect if not specified)')
    parser.add_argument('--check-spelling', action='store_true', help='Check for spelling errors')
    parser.add_argument('--output-dir', help='Output directory (default: same as input)')
    
    args = parser.parse_args()
    
    # Get API key - prioritize new env var, fallback to OPENAI_API_KEY
    api_key = args.api_key or os.environ.get('AI_Model_API_KEY') or os.environ.get('OPENAI_API_KEY')
    if not api_key:
        print("Error: API key required. Set AI_Model_API_KEY or OPENAI_API_KEY env var, or use --api-key")
        sys.exit(1)
    
    # Get base URL for alternative providers (optional)
    base_url = args.base_url or os.environ.get('AI_Model_BASE_URL')
    
    # Get model name - prioritize new env var, fallback to default
    model = args.model or os.environ.get('AI_Model_Name') or DEFAULT_MODEL
    
    # Initialize translator
    translator = QMDTranslator(api_key=api_key, model=model, base_url=base_url)
    
    # Process each file
    success_count = 0
    for input_file in args.input_files:
        if not os.path.exists(input_file):
            print(f"Warning: File not found: {input_file}")
            continue
        
        # Determine output path
        if args.output_dir:
            output_file = os.path.join(args.output_dir, os.path.basename(get_translation_pair(input_file)))
        else:
            output_file = get_translation_pair(input_file)
        
        # Check spelling if requested
        if args.check_spelling:
            print(f"\nChecking spelling in {input_file}...")
            issues = translator.check_spelling(input_file)
            if issues:
                print("⚠ Potential issues found:")
                for issue in issues:
                    print(f"  - {issue}")
            else:
                print("✓ No issues found")
            # Continue to next file without translating when only checking spelling
            continue
        
        # Translate
        if translator.translate_qmd_file(input_file, output_file, args.target_lang):
            success_count += 1
    
    print(f"\n✓ Successfully translated {success_count}/{len(args.input_files)} files")
    sys.exit(0 if success_count == len(args.input_files) else 1)


if __name__ == '__main__':
    main()

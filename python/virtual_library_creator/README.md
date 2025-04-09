# Virtual Library Creator *(formerly [Piranesi](https://github.com/gongahkia/piranesi))*

## Things to follow up on and add

The below were migrated over from [Piranesi](https://github.com/gongahkia/piranesi).

1. barcode scanner functionality CLI tool vanilla implementation
    * every commercial book ever produced has a barcode somewhere on its front or back, so write a barcode scanner that when given a cover, will extract the barcode and scan it, then output the scanned results to a json, and extract relevant information like the book's ISBN to readily search it via openlibrary's api which i have code for under retrieve.py
2. 3d model export functionality
    * consider adding ADDED functionality which allows for users to instead of generating a library, allows users to scan all edges of their book, then convert it to a FLAT SPLAYED OUT visualistion of the entire cover (cover, spine, back), then provide exporting to common 3d formats especially blender and others
3. CLI tool vanilla implementation
    * us on **completing the existing workflow** where users can crop the image themselves and automate the process after that to generating a HTML component that can work when hovered over and can be readily embedded into user's websites
4. book spine generation
    * consider integrating a locally-run or API-called LLM to generate what it thinks the spine of that book should look like, including multiple safeguards to ensure the image generated isn't too offensive 
    * consider integrating a VECTOR DATABASE to store existing book spine covers, as visual reference for the LLM to then generate new bookcovers
5. broken API extracting incomplete details
    * add more details that I want to extract, such as book details and book description and tags associated with the book
6. broken edge detection
    * incomplete coverage of all types of images, consider adding **preprocesssing** steps to make cover edges easier to recognise
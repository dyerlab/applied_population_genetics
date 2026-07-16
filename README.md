# Applied Population Genetics

This is the repository for the textbook **Applied Population Genetics** by Rodney J. Dyer. 

While the text and content continue to be refined, the book is freely available and hosted online at:

[http://dyerlab.github.io/applied_population_genetics/index.html](http://dyerlab.github.io/applied_population_genetics/index.html)

Any comments, suggestions, or contributions should be sent to Rodney J. Dyer at [rjdyer@vcu.edu](mailto:rjdyer@vcu.edu). Additional research and work from the Dyer Laboratory can be found at [http://dyerlab.org](http://dyerlab.org).

---

## Recent Modernizations & Migrations

The textbook code and examples have recently undergone major system-wide updates to align with the modern R spatial and visualization ecosystems:

1. **Vector Spatial Data Migration**:
   - Replaced legacy, retired packages `sp` and `rgdal` with the modern industry standards **`sf`** (Simple Features) and **`terra`**.
2. **Raster Spatial Data Migration**:
   - Transitioned from the legacy `raster` package to **`terra`**, which provides faster, robust, and unified handling of raster and vector spatial data.
3. **Interactive Maps & Visualizations**:
   - Migrated mapping examples from `ggmap` / Google Maps APIs (which often require proprietary API keys and present static limitations) to **`leaflet`** for modern, interactive, and client-side web mapping.
4. **Library & Dependency Updates**:
   - Upgraded all package dependencies to their most recent versions to ensure long-term reproducibility and compatibility with current R environments.
5. **Recompiled HTML Output**:
   - Fully built and compiled the textbook using the latest Quarto engine, with the generated HTML textbook pages available in the `docs/` directory for GitHub Pages hosting.

---

## Technical Details & Build Instructions

This textbook is constructed as a **Quarto Book**. 

### Repository Structure
- **`*.qmd`**: Chapter files containing the narrative text, math (LaTeX), and executable R code blocks.
- **`_quarto.yml`**: Book configuration file specifying the chapters, theme, and rendering options.
- **`docs/`**: The rendered, publication-ready HTML pages served via GitHub Pages.
- **`media/`** & **`chapter_images/`**: High-quality graphics, figures, and cover assets.
- **`data/`**: Datasets (e.g., `Cornus.csv`) used throughout the practical exercises.

### Building the Book Locally
To render the book locally on your machine, you must have [Quarto](https://quarto.org) installed along with a standard R environment.

1. **Install Quarto**: Follow the instructions on the [Quarto website](https://quarto.org/docs/get-started/).
2. **Install R Dependencies**: Open R and ensure you have the updated core spatial and plotting libraries installed:
   ```R
   install.packages(c("sf", "terra", "leaflet", "gstudio", "popgraph", "tidyverse", "quarto"))
   ```
3. **Render the Book**: Run the following command in the project root directory:
   ```bash
   quarto render
   ```
   This will execute all code blocks, compile the `.qmd` files, and place the output in the `docs/` directory.

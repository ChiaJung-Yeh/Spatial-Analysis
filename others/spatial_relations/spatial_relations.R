# ============================================================
# Spatial Predicate Demonstration
# ============================================================

library(sf)
library(ggplot2)
library(dplyr)
library(base64enc)
library(htmltools)


# ============================================================
# 1. Helper functions
# ============================================================

make_circle <- function(x, y, radius = 1) {
  
  st_buffer(
    st_point(c(x, y)),
    dist = radius
  )
  
}


make_line <- function(x1, y1, x2, y2) {
  
  st_linestring(
    matrix(
      c(
        x1, y1,
        x2, y2
      ),
      ncol = 2,
      byrow = TRUE
    )
  ) |>
    st_sfc()
  
}


make_point <- function(x, y) {
  
  st_sfc(
    st_point(c(x, y))
  )
  
}


# ============================================================
# 2. Define examples
# ============================================================

examples <- list(
  
  # ----------------------------------------------------------
  # Polygon - Polygon
  # ----------------------------------------------------------
  
  "Overlapping circles" = list(
    
    A = make_circle(-0.5, 0, 1),
    
    B = make_circle(0.5, 0, 1),
    
    Definition =
      "The two polygons partially share the same area, but neither completely contains the other.",
    
    Code = '
circle1 <- st_buffer(
  st_point(c(0.5, 0)),
  1
)

circle2 <- st_buffer(
  st_point(c(0.5, 0)),
  1
)

st_overlaps(circle1, circle2)
'
  ),


"Touching circles" = list(
  
  A = make_circle(-1, 0, 1),
  
  B = make_circle(1, 0, 1),
  
  Definition =
    "The polygon boundaries meet, but their interiors do not overlap.",
  
  Code = '
circle1 <- st_buffer(
  st_point(c(-1, 0)),
  1
)

circle2 <- st_buffer(
  st_point(c(1, 0)),
  1
)

st_touches(circle1, circle2)
'
),


"Disjoint circles" = list(
  
  A = make_circle(-1.5, 0, 1),
  
  B = make_circle(1.5, 0, 1),
  
  Definition =
    "The two polygons have no points in common.",
  
  Code = '
circle1 <- st_buffer(
  st_point(c(-1.5, 0)),
  1
)

circle2 <- st_buffer(
  st_point(c(1.5, 0)),
  1
)

st_disjoint(circle1, circle2)
'
),


"A within B" = list(
  
  A = make_circle(0, 0, 0.7),
  
  B = make_circle(0, 0, 1.5),
  
  Definition =
    "Geometry A lies completely inside geometry B.",
  
  Code = '
A <- st_buffer(
  st_point(c(0, 0)),
  0.7
)

B <- st_buffer(
  st_point(c(0, 0)),
  1.5
)

st_within(A, B)
'
),


"A contains B" = list(
  
  A = make_circle(0, 0, 1.5),
  
  B = make_circle(0, 0, 0.7),
  
  Definition =
    "Geometry A completely contains geometry B.",
  
  Code = '
A <- st_buffer(
  st_point(c(0, 0)),
  1.5
)

B <- st_buffer(
  st_point(c(0, 0)),
  0.7
)

st_contains(A, B)
'
),


# ----------------------------------------------------------
# Line - Line
# ----------------------------------------------------------

"Crossing lines" = list(
  
  A = make_line(
    -1, -1,
    1,  1
  ),
  
  B = make_line(
    -1,  1,
    1, -1
  ),
  
  Definition =
    "The two lines pass through each other at an interior point.",
  
  Code = '
line1 <- st_linestring(
  matrix(
    c(
      -1, -1,
       1,  1
    ),
    ncol = 2,
    byrow = TRUE
  )
) |> st_sfc()

line2 <- st_linestring(
  matrix(
    c(
      -1,  1,
       1, -1
    ),
    ncol = 2,
    byrow = TRUE
  )
) |> st_sfc()

st_crosses(line1, line2)
'
),


# ----------------------------------------------------------
# Line - Polygon
# ----------------------------------------------------------

"Line crossing circle" = list(
  
  A = make_line(
    -2, 0,
    2, 0
  ),
  
  B = make_circle(
    0, 0, 1
  ),
  
  Definition =
    "The line enters and exits the polygon, passing through its interior.",
  
  Code = '
line <- st_linestring(
  matrix(
    c(
      -2, 0,
       2, 0
    ),
    ncol = 2,
    byrow = TRUE
  )
) |> st_sfc()

circle <- st_buffer(
  st_point(c(0, 0)),
  1
)

st_crosses(line, circle)
'
),


# ----------------------------------------------------------
# Point - Polygon
# ----------------------------------------------------------

"Point within circle" = list(
  
  A = make_point(
    0, 0
  ),
  
  B = make_circle(
    0, 0, 1
  ),
  
  Definition =
    "The point lies inside the interior of the polygon.",
  
  Code = '
point <- st_sfc(
  st_point(c(0, 0))
)

circle <- st_buffer(
  st_point(c(0, 0)),
  1
)

st_within(point, circle)
'
),


"Point outside circle" = list(
  
  A = make_point(
    2, 0
  ),
  
  B = make_circle(
    0, 0, 1
  ),
  
  Definition =
    "The point lies outside the polygon and shares no point with it.",
  
  Code = '
point <- st_sfc(
  st_point(c(2, 0))
)

circle <- st_buffer(
  st_point(c(0, 0)),
  1
)

st_disjoint(point, circle)
'
),


"Point on circle boundary" = list(
  
  A = make_point(
    1, 0
  ),
  
  B = make_circle(
    0, 0, 1
  ),
  
  Definition =
    "The point lies on the boundary of the polygon but not in its interior.",
  
  Code = '
point <- st_sfc(
  st_point(c(1, 0))
)

circle <- st_buffer(
  st_point(c(0, 0)),
  1
)

st_touches(point, circle)
'
),


"Circle contains point" = list(
  
  A = make_circle(
    0, 0, 1
  ),
  
  B = make_point(
    0, 0
  ),
  
  Definition =
    "The polygon contains the point in its interior.",
  
  Code = '
circle <- st_buffer(
  st_point(c(0, 0)),
  1
)

point <- st_sfc(
  st_point(c(0, 0))
)

st_contains(circle, point)
'
),


# ----------------------------------------------------------
# Point - Line
# ----------------------------------------------------------

"Point on line interior" = list(
  
  A = make_point(
    0, 0
  ),
  
  B = make_line(
    -2, 0,
    2, 0
  ),
  
  Definition =
    "The point lies on the interior of the line.",
  
  Code = '
point <- st_sfc(
  st_point(c(0, 0))
)

line <- st_linestring(
  matrix(
    c(
      -2, 0,
       2, 0
    ),
    ncol = 2,
    byrow = TRUE
  )
) |> st_sfc()

st_intersects(point, line)
'
),


"Point touching line endpoint" = list(
  
  A = make_point(
    2, 0
  ),
  
  B = make_line(
    -2, 0,
    2, 0
  ),
  
  Definition =
    "The point coincides with an endpoint, which forms part of the line boundary.",
  
  Code = '
point <- st_sfc(
  st_point(c(2, 0))
)

line <- st_linestring(
  matrix(
    c(
      -2, 0,
       2, 0
    ),
    ncol = 2,
    byrow = TRUE
  )
) |> st_sfc()

st_touches(point, line)
'
),


# ----------------------------------------------------------
# Point - Point
# ----------------------------------------------------------

"Coincident points" = list(
  
  A = make_point(
    0, 0
  ),
  
  B = make_point(
    0, 0
  ),
  
  Definition =
    "The two points occupy exactly the same location.",
  
  Code = '
point1 <- st_sfc(
  st_point(c(0, 0))
)

point2 <- st_sfc(
  st_point(c(0, 0))
)

st_intersects(point1, point2)
'
),


"Separated points" = list(
  
  A = make_point(
    0, 0
  ),
  
  B = make_point(
    1, 1
  ),
  
  Definition =
    "The two points occupy different locations.",
  
  Code = '
point1 <- st_sfc(
  st_point(c(0, 0))
)

point2 <- st_sfc(
  st_point(c(1, 1))
)

st_disjoint(point1, point2)
'
)
)


# ============================================================
# 3. Function to evaluate all spatial predicates
# ============================================================

test_predicates <- function(
    name,
    A,
    B,
    definition,
    code
) {
  
  data.frame(
    
    Relationship = name,
    
    Definition = definition,
    
    Intersects =
      st_intersects(
        A,
        B,
        sparse = FALSE
      )[1, 1],
    
    Disjoint =
      st_disjoint(
        A,
        B,
        sparse = FALSE
      )[1, 1],
    
    Touches =
      st_touches(
        A,
        B,
        sparse = FALSE
      )[1, 1],
    
    Overlaps =
      st_overlaps(
        A,
        B,
        sparse = FALSE
      )[1, 1],
    
    Within =
      st_within(
        A,
        B,
        sparse = FALSE
      )[1, 1],
    
    Contains =
      st_contains(
        A,
        B,
        sparse = FALSE
      )[1, 1],
    
    Crosses =
      st_crosses(
        A,
        B,
        sparse = FALSE
      )[1, 1],
    
    Code = code,
    
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 4. Run all predicates
# ============================================================

result <- bind_rows(
  
  lapply(
    
    names(examples),
    
    function(name) {
      
      test_predicates(
        
        name,
        
        examples[[name]]$A,
        
        examples[[name]]$B,
        
        examples[[name]]$Definition,
        
        examples[[name]]$Code
        
      )
      
    }
    
  )
  
)


# ============================================================
# 5. Create figure folder
# ============================================================

dir.create(
  "spatial_figures",
  showWarnings = FALSE
)


figure_files <- character(
  length(examples)
)

# ============================================================
# 6. Generate geometry figures
# Draw polygons/lines first, points last
# ============================================================

for (i in seq_along(examples)) {
  
  A <- examples[[i]]$A
  B <- examples[[i]]$B
  
  type_A <- as.character(
    st_geometry_type(A)
  )
  
  type_B <- as.character(
    st_geometry_type(B)
  )
  
  p <- ggplot()
  
  
  # ==========================================================
  # STEP 1: Draw POLYGONS first
  # ==========================================================
  
  if (grepl("POLYGON", type_A)) {
    
    p <- p +
      geom_sf(
        data = A,
        fill = "lightblue",
        alpha = 0.60,
        linewidth = 1
      )
  }
  
  if (grepl("POLYGON", type_B)) {
    
    p <- p +
      geom_sf(
        data = B,
        fill = "orange",
        alpha = 0.45,
        linewidth = 1
      )
  }
  
  
  # ==========================================================
  # STEP 2: Draw LINES second
  # ==========================================================
  
  if (grepl("LINESTRING", type_A)) {
    
    p <- p +
      geom_sf(
        data = A,
        colour = "black",
        linewidth = 1.8,
        linetype = "solid"
      )
  }
  
  
  if (grepl("LINESTRING", type_B)) {
    
    # For point-line examples, use a solid black line.
    # Otherwise keep B dashed for line-line comparison.
    
    if (
      examples[[i]]$Definition %in% c(
        "The point lies on the interior of the line.",
        "The point coincides with an endpoint, which forms part of the line boundary."
      )
    ) {
      
      p <- p +
        geom_sf(
          data = B,
          colour = "black",
          linewidth = 2.2,
          linetype = "solid"
        )
      
    } else {
      
      p <- p +
        geom_sf(
          data = B,
          colour = "black",
          linewidth = 1.8,
          linetype = "dashed"
        )
    }
  }
  
  
  # ==========================================================
  # STEP 3: Draw POINTS LAST
  # This prevents points from being hidden by lines/polygons
  # ==========================================================
  
  if (grepl("POINT", type_A)) {
    
    p <- p +
      geom_sf(
        data = A,
        size = 5,
        shape = 21,
        fill = "lightblue",
        stroke = 1.3
      )
  }
  
  if (grepl("POINT", type_B)) {
    
    p <- p +
      geom_sf(
        data = B,
        size = 5,
        shape = 21,
        fill = "orange",
        stroke = 1.3
      )
  }
  
  
  # ==========================================================
  # Plot styling
  # ==========================================================
  
  p <- p +
    coord_sf(
      xlim = c(-3, 3),
      ylim = c(-2, 2),
      expand = FALSE,
      datum = NA
    )+
    theme_void() +
    theme(
      plot.margin = margin(
        5, 5, 5, 5
      )
    )
  
  
  # ==========================================================
  # Save figure
  # ==========================================================
  
  file_name <- paste0(
    "spatial_figures/",
    "figure_",
    i,
    ".png"
  )
  
  ggsave(
    filename = file_name,
    plot = p,
    width = 2.2,
    height = 1.7,
    dpi = 150,
    bg = "white"
  )
  
  figure_files[i] <- file_name
}

# ============================================================
# 7. Add figures
# ============================================================

result$Figure <-
  figure_files


# ============================================================
# 8. Convert TRUE/FALSE into symbols
# ============================================================

predicate_columns <- c(
  
  "Intersects",
  
  "Disjoint",
  
  "Touches",
  
  "Overlaps",
  
  "Within",
  
  "Contains",
  
  "Crosses"
  
)


result[predicate_columns] <-
  
  lapply(
    
    result[predicate_columns],
    
    function(x) {
      
      ifelse(
        x,
        "✓",
        "✕"
      )
      
    }
    
  )


# ============================================================
# 9. Convert images to Base64
# This makes the HTML self-contained
# ============================================================

image_to_data_uri <- function(path) {
  
  base64enc::dataURI(
    
    file = path,
    
    mime = "image/png"
    
  )
  
}


result$ImageURI <-
  
  sapply(
    
    result$Figure,
    
    image_to_data_uri
    
  )


# ============================================================
# 10. HTML escape helper
# ============================================================

esc <- function(x) {
  
  as.character(
    
    htmltools::htmlEscape(
      x
    )
    
  )
  
}


# ============================================================
# 11. Build individual HTML rows
# ============================================================

rows_html <-
  character(
    nrow(result)
  )


for (
  i in seq_len(
    nrow(result)
  )
) {
  
  # ----------------------------------------------------------
  # Encode R code for safe storage
  # ----------------------------------------------------------
  
  code_encoded <-
    
    URLencode(
      
      result$Code[i],
      
      reserved = TRUE
      
    )
  
  
  # ----------------------------------------------------------
  # Determine symbol classes
  # ----------------------------------------------------------
  
  predicate_cell <- function(value) {
    
    if (value == "✓") {
      
      paste0(
        "<td class='predicate-cell true-cell'>",
        value,
        "</td>"
      )
      
    } else {
      
      paste0(
        "<td class='predicate-cell false-cell'>",
        value,
        "</td>"
      )
      
    }
    
  }
  
  
  # ----------------------------------------------------------
  # HTML row
  # ----------------------------------------------------------
  
  rows_html[i] <- paste0(
    
    "<tr>",
    
    
    # Geometry
    "<td class='geometry-cell'>",
    
    "<img src='",
    result$ImageURI[i],
    "' class='geometry-img'>",
    
    "</td>",
    
    
    # Relationship
    "<td class='relationship-cell'>",
    
    esc(
      result$Relationship[i]
    ),
    
    "</td>",
    
    
    # Definition
    "<td class='definition-cell'>",
    
    esc(
      result$Definition[i]
    ),
    
    "</td>",
    
    
    # Predicates
    predicate_cell(
      result$Intersects[i]
    ),
    
    predicate_cell(
      result$Disjoint[i]
    ),
    
    predicate_cell(
      result$Touches[i]
    ),
    
    predicate_cell(
      result$Overlaps[i]
    ),
    
    predicate_cell(
      result$Within[i]
    ),
    
    predicate_cell(
      result$Contains[i]
    ),
    
    predicate_cell(
      result$Crosses[i]
    ),
    
    
    # R code button
    "<td class='code-cell'>",
    
    "<button ",
    
    "class='code-btn' ",
    
    "data-title='",
    esc(
      result$Relationship[i]
    ),
    "' ",
    
    "data-code='",
    code_encoded,
    "' ",
    
    "onclick='openCodeModal(this)'>",
    
    "View code",
    
    "</button>",
    
    "</td>",
    
    
    "</tr>"
    
  )
  
}


# ============================================================
# 12. Build complete HTML document
# ============================================================

html_output <- paste0(
  
  '<!DOCTYPE html>

<html>

<head>

<meta charset="UTF-8">

<meta
  name="viewport"
  content="width=device-width, initial-scale=1.0"
>

<title>
Spatial Relationships
</title>


<style>


/* =========================================================
   GLOBAL
   ========================================================= */

html,
body,
table,
button,
input {

  font-family:
    Georgia,
    "Times New Roman",
    Times,
    serif;

}


body {

  margin: 30px;

  background: #ffffff;

  color: #222222;

  line-height: 1.5;

}


/* =========================================================
   TITLE
   ========================================================= */

h1 {

  margin-bottom: 4px;

  font-size: 30px;

  font-weight: 600;

}


.subtitle {

  color: #666666;

  margin-bottom: 25px;

  font-size: 16px;

}


/* =========================================================
   TABLE WRAPPER
   ========================================================= */

.table-wrapper {

  width: 100%;

  overflow-x: auto;

}


/* =========================================================
   TABLE
   ========================================================= */

table {

  border-collapse: collapse;

  width: 100%;

  min-width: 1300px;

  font-size: 15px;

  background: white;

}


th {

  background: #f1f5f9;

  color: #222222;

  font-weight: 600;

  padding: 11px 9px;

  border: 1px solid #d1d5db;

  text-align: center;

  vertical-align: middle;

  white-space: nowrap;

}


td {

  padding: 10px;

  border: 1px solid #d1d5db;

  text-align: center;

  vertical-align: middle;

}


tbody tr:hover {

  background: #f8fafc;

}


/* =========================================================
   RELATIONSHIP / DEFINITION
   ========================================================= */

.relationship-cell {

  text-align: left;

  font-weight: 600;

  min-width: 170px;

}


.definition-cell {

  text-align: left;

  min-width: 300px;

  max-width: 380px;

  line-height: 1.45;

}


/* =========================================================
   GEOMETRY
   ========================================================= */

.geometry-cell {

  min-width: 160px;

}


.geometry-img {

  height: 105px;

  max-width: 150px;

  display: block;

  margin: auto;

}


/* =========================================================
   PREDICATE CELLS
   ========================================================= */

.predicate-cell {

  min-width: 72px;

  font-size: 18px;

}


.true-cell {

  font-weight: 700;

}


.false-cell {

  color: #777777;

}


/* =========================================================
   CODE BUTTON
   ========================================================= */

.code-cell {

  min-width: 110px;

}


.code-btn {

  font-family:
    Georgia,
    "Times New Roman",
    Times,
    serif;

  background: #2563eb;

  color: white;

  border: none;

  border-radius: 6px;

  padding: 8px 14px;

  cursor: pointer;

  font-size: 14px;

  transition:
    background 0.15s ease,
    transform 0.15s ease;

}


.code-btn:hover {

  background: #1d4ed8;

  transform: translateY(-1px);

}


.code-btn:active {

  transform: translateY(0);

}


/* =========================================================
   MODAL BACKGROUND
   ========================================================= */

.modal {

  display: none;

  position: fixed;

  z-index: 9999;

  left: 0;

  top: 0;

  width: 100%;

  height: 100%;

  overflow: auto;

  background-color:
    rgba(0, 0, 0, 0.50);

  padding-top: 40px;

}


/* =========================================================
   MODAL WINDOW
   ========================================================= */

.modal-content {

  background-color: #ffffff;

  margin: auto;

  padding: 22px;

  border-radius: 10px;

  width: 70%;

  max-width: 850px;

  box-shadow:
    0 12px 35px
    rgba(0, 0, 0, 0.28);

  animation:
    modalFade 0.15s ease-out;

}


@keyframes modalFade {

  from {

    opacity: 0;

    transform:
      translateY(-8px);

  }

  to {

    opacity: 1;

    transform:
      translateY(0);

  }

}


/* =========================================================
   MODAL HEADER
   ========================================================= */

.modal-header {

  display: flex;

  justify-content:
    space-between;

  align-items: center;

  border-bottom:
    1px solid #e5e7eb;

  padding-bottom: 10px;

  margin-bottom: 15px;

}


.modal-title {

  font-size: 21px;

  font-weight: 600;

}


.close-button {

  font-size: 28px;

  font-weight: bold;

  cursor: pointer;

  color: #555555;

  line-height: 1;

}


.close-button:hover {

  color: #000000;

}


/* =========================================================
   CODE
   ========================================================= */

pre {

  font-family:
    Georgia,
    "Times New Roman",
    Times,
    serif;

  background: #f1f5f9;

  padding: 18px;

  border-radius: 8px;

  overflow-x: auto;

  text-align: left;

  font-size: 15px;

  line-height: 1.55;

  white-space: pre;

  margin-bottom: 0;

}


code {

  font-family:
    Georgia,
    "Times New Roman",
    Times,
    serif;

}


/* =========================================================
   RESPONSIVE
   ========================================================= */

@media (max-width: 900px) {

  body {

    margin: 15px;

  }


  .modal-content {

    width: 90%;

  }


  h1 {

    font-size: 25px;

  }

}

</style>

</head>


<body>


<h1>
Spatial Predicate Relationships
</h1>


<div class="subtitle">

Examples of common spatial relationships using the R
<em>sf</em> package

</div>


<div class="table-wrapper">


<table>


<thead>

<tr>

<th>
Geometry
</th>

<th>
Relationship
</th>

<th>
Definition
</th>

<th>
Intersects
</th>

<th>
Disjoint
</th>

<th>
Touches
</th>

<th>
Overlaps
</th>

<th>
Within
</th>

<th>
Contains
</th>

<th>
Crosses
</th>

<th>
R Code
</th>

</tr>

</thead>


<tbody>',

paste(
  rows_html,
  collapse = "\n"
),

'</tbody>


</table>


</div>


<!-- ======================================================
     CODE MODAL
     ====================================================== -->

<div
  id="codeModal"
  class="modal"
>


<div class="modal-content">


<div class="modal-header">


<div
  id="modalTitle"
  class="modal-title"
>

R Code

</div>


<span
  class="close-button"
  onclick="closeCodeModal()"
>

&times;

</span>


</div>


<pre><code id="modalCode"></code></pre>


</div>


</div>


<!-- ======================================================
     JAVASCRIPT
     ====================================================== -->

<script>


function openCodeModal(button) {

  const encodedCode =
    button.getAttribute(
      "data-code"
    );


  const relationship =
    button.getAttribute(
      "data-title"
    );


  const code =
    decodeURIComponent(
      encodedCode
    );


  document.getElementById(
    "modalTitle"
  ).textContent =
    relationship +
    " — R Code";


  document.getElementById(
    "modalCode"
  ).textContent =
    code;


  document.getElementById(
    "codeModal"
  ).style.display =
    "block";

}


function closeCodeModal() {

  document.getElementById(
    "codeModal"
  ).style.display =
    "none";

}


/* Close modal when clicking outside */

window.addEventListener(

  "click",

  function(event) {

    const modal =
      document.getElementById(
        "codeModal"
      );


    if (
      event.target === modal
    ) {

      closeCodeModal();

    }

  }

);


/* Close modal using Escape key */

document.addEventListener(

  "keydown",

  function(event) {

    if (
      event.key === "Escape"
    ) {

      closeCodeModal();

    }

  }

);


</script>


</body>

</html>'

)


# ============================================================
# 13. Export HTML
# ============================================================

writeLines(
  
  html_output,
  
  "spatial_predicates_table.html"
  
)


# ============================================================
# 14. Open HTML automatically
# ============================================================

browseURL(
  
  normalizePath(
    "spatial_predicates_table.html"
  )
  
)
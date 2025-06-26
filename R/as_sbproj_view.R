#' Convert an object into an "sbproj_view" object
#'
#' @param x The object to convert
#' @param ... Ignored
#' @returns An "sbproj_view" object
#' @export
as_sbproj_view <- function(x, ...) {
  UseMethod("as_sbproj_view")
}

#' @export
as_sbproj_view.xml_document <- function(x, ...) {
  as_sbproj_view(set_nodeclass(x))
}

# Set the class of an XML object to its node class
set_nodeclass <- function(x) {
  node_class <- xml2::xml_attr(x, "class")
  if (!is.na(node_class)) {
    class(x) <- c(paste0("xml_", xml2::xml_name(x), "_", node_class), setdiff(class(x), "xml_document"))
  } else if (xml2::xml_name(x) == "void") {
    node_prop <- xml2::xml_attr(x, "property")
    node_method <- xml2::xml_attr(x, "method")
    if (!is.na(node_prop)) {
      class(x) <- c(paste0("xml_", xml2::xml_name(x), "_prop", node_prop), setdiff(class(x), "xml_document"))
    } else if (!is.na(node_method)) {
      class(x) <- c(paste0("xml_", xml2::xml_name(x), "_meth", node_method), setdiff(class(x), "xml_document"))
    } else {
      stop("Please report a bug: <void> tag without property or method attribute") # nocov
    }
  } else {
    class(x) <- c(paste0("xml_", xml2::xml_name(x)), setdiff(class(x), "xml_document"))
  }
  x
}

#' @export
as_sbproj_view.xml_java_java.beans.XMLDecoder <- function(x, ...) {
  children <- xml2::xml_children(x)
  stopifnot(length(children) == 1)
  as_sbproj_view(set_nodeclass(children[[1]]), ...)
}

#' @export
as_sbproj_view.xml_object_com.mathworks.toolbox.simbio.desktop.editor.components.SimBiologyDiagramView <- function(x, ...) {
  children <- lapply(X = xml2::xml_children(x), FUN = set_nodeclass)
  ret <-
    append(
      list(id = xml2::xml_attr(x, "id")),
      lapply(X = children, FUN = as_sbproj_view, ...)
    )
  # Drop unimportant elements
  ret <- ret[!vapply(X = ret, FUN = is.null, FUN.VALUE = NA)]
  class(ret) <- "sbproj_view"
  ret
}

#' @export
as_sbproj_view.xml_void_propdiagram <- function(x, ...) {
  add_single_element(x, ...)
}

#' @export
as_sbproj_view.xml_void_propscale <- function(x, ...) {
  # This appears to be for viewing, only
  NULL
}

#' @export
as_sbproj_view.xml_void_methaddMouseMotionListener <- function(x, ...) {
  # This appears to be for managing SimBiology state, only
  NULL
}

#' @export
as_sbproj_view.xml_void_methaddFocusListener <- function(x, ...) {
  # This appears to be for managing SimBiology state, only
  NULL
}

#' @export
as_sbproj_view.xml_void_methadd <- function(x, ...) {
  add_single_element(x, ...)
}

#' @export
as_sbproj_view.xml_object_com.mathworks.toolbox.simbio.desktop.editor.SimBiologyDiagram <- function(x, ...) {
  add_multiple_elements(x, ...)
}

#' @export
as_sbproj_view.xml_object_com.mathworks.toolbox.simbio.desktop.editor.blocks.SpeciesBlock <- function(x, ...) {
  id <- xml2::xml_attr(x, "id")
  stopifnot(!is.na(id))
  info <- add_multiple_elements(x, ...)
  # sbproj_view_index may not be present
  stopifnot(length(info) %in% c(4, 5))
  ret <-
    list(
      id = id,
      identifier = get_class_from_list(info, "sbproj_view_identifier", len = 1),
      bounds = get_class_from_list(info, "sbproj_propbounds", len = 1),
      name = get_class_from_list(info, "sbproj_view_identifier", len = 1),
      compartment = get_class_from_list(info, "sbproj_view_compartment", len = 1),
      index = get_class_from_list(info, "sbproj_view_index", len = c(0, 1))
    )
  class(ret) <- c("sbproj_view_speciesblock", "sbproj_view")
  ret
}

#' @export
as_sbproj_view.xml_void_propbounds <- function(x, ...) {
  info <- add_multiple_elements(x, ...)
  ids <- vapply(X = info, FUN = getElement, name = "id", FUN.VALUE = "")
  dimensions <- vapply(X = info, FUN = getElement, name = "dimension", FUN.VALUE = "")
  positions <- vapply(X = info, FUN = getElement, name = "position", FUN.VALUE = 1)
  ret <-
    list(
      id = ids[[1]],
      position = stats::setNames(positions, nm = dimensions)
    )
  class(ret) <- c("sbproj_propbounds", class(info[[1]]))
  ret
}

#' @export
as_sbproj_view.xml_void_java.awt.Rectangle <- function(x, ...) {
  info <- add_multiple_elements(x, ...)
  stopifnot(is.character(info[[1]]))
  stopifnot(is.numeric(info[[2]]))
  ret <-
    list(
      id = names(info[[2]]),
      dimension = info[[1]],
      position = info[[2]][[1]]
    )
  class(ret) <- c("sbproj_view_Rectangle", "sbproj_view")
  ret
}

#' @export
as_sbproj_view.xml_string <- function(x, ...) {
  as.character(xml2::xml_contents(x))
}

#' @export
as_sbproj_view.xml_void_methset <- function(x, ...) {
  # Get the value of child attributes
  children <- xml2::xml_children(x)
  stopifnot(length(children) == 2)
  stopifnot(xml2::xml_name(children[[1]]) == "object")
  ret <- as_sbproj_view(set_nodeclass(children[[2]]), ...)
  stats::setNames(ret, xml2::xml_attr(children[[1]], "idref"))
}

#' @export
as_sbproj_view.xml_int <- function(x, ...) {
  as.integer(as.character(xml2::xml_contents(x)[[1]]))
}

#' @export
as_sbproj_view.xml_void_propidentifier <- function(x, ...) {
  assert_xml_children(x, len = 1)
  ret <- as_sbproj_view(set_nodeclass(xml2::xml_child(x)), ...)
  class(ret) <- c("sbproj_view_identifier", "sbproj_view")
  ret
}

#' @export
as_sbproj_view.xml_void_propname <- function(x, ...) {
  assert_xml_children(x, len = 1)
  ret <- as_sbproj_view(set_nodeclass(xml2::xml_child(x)), ...)
  class(ret) <- c("sbproj_view_name", "sbproj_view")
  ret
}

#' @export
as_sbproj_view.xml_void_propheight <- function(x, ...) {
  assert_xml_children(x, len = 1)
  ret <- as_sbproj_view(set_nodeclass(xml2::xml_child(x)), ...)
  class(ret) <- c("sbproj_view_height", "sbproj_view")
  ret
}

#' @export
as_sbproj_view.xml_void_propwidth <- function(x, ...) {
  assert_xml_children(x, len = 1)
  ret <- as_sbproj_view(set_nodeclass(xml2::xml_child(x)), ...)
  class(ret) <- c("sbproj_view_width", "sbproj_view")
  ret
}

#' @export
as_sbproj_view.xml_void_propcompartment <- function(x, ...) {
  assert_xml_children(x, len = 1)
  child <- xml2::xml_child(x)
  assert_xml_name(child, "object")
  idref <- xml2::xml_attr(child, "idref")
  id <- xml2::xml_attr(child, "id")
  if (!xor(is.na(idref), is.na(id))) {
    stop("Please report a bug: Compartment idref and id cannot both be NA") # nocov
  } else if (!is.na(idref)) {
    ret <- idref
    class(ret) <- c("sbproj_view_compartmentref", "sbproj_view_compartment", "sbproj_view")
  } else if (!is.na(id)) {
    ret <- as_sbproj_view(set_nodeclass(child), ...)
  } else {
    stop("Please report a bug: Compartment idref and id cannot both be NA (check 2)") # nocov
  }
  ret
}

#' @export
as_sbproj_view.xml_object_com.mathworks.toolbox.simbio.desktop.editor.blocks.SimBiologyCompartmentBlock <- function(x, ...) {
  info <- add_multiple_elements(x, ...)
  stopifnot(length(info) == 3)
  ret <-
    list(
      identifier = get_class_from_list(info, "sbproj_view_identifier", len = 1),
      name = get_class_from_list(info, "sbproj_view_name", len = 1),
      bounds = get_class_from_list(info, "sbproj_propbounds", len = 1)
    )
  class(ret) <- c("sbproj_view_compartment", "sbproj_view")
  ret
}

#' @export
as_sbproj_view.xml_object_com.mathworks.toolbox.simbio.desktop.editor.blocks.ReactionBlock <- function(x, ...) {
  info <- add_multiple_elements(x, ...)
  # sbproj_view_index may be missing
  stopifnot(length(info) %in% c(3, 4))
  ret <-
    list(
      id = xml2::xml_attr(x, "id"),
      bounds = get_class_from_list(info, "sbproj_propbounds", len = 1),
      identifier = get_class_from_list(info, "sbproj_view_identifier", len = 1),
      name = get_class_from_list(info, "sbproj_view_name", len = 1),
      index = get_class_from_list(info, "sbproj_view_index", len = c(0, 1))
    )
  class(ret) <- c("sbproj_view_reactionblock", "sbproj_view")
  ret
}

#' @export
as_sbproj_view.xml_object_com.mathworks.toolbox.simbio.desktop.editor.SimBiologyLine <- function(x, ...) {
  info <- add_multiple_elements(x, ...)
  stopifnot(length(info) %in% (3:5))
  ret <-
    list(
      id = xml2::xml_attr(x, "id"),
      container = get_class_from_list(info, "sbproj_view_objectref", what_not = c("sbproj_view_startconnection", "sbproj_view_endconnection"), len = 1),
      start = get_class_from_list(info, "sbproj_view_startconnection", len = 1),
      start_terminator = get_class_from_list(info, "sbproj_view_startterminator", len = c(0, 1)),
      end = get_class_from_list(info, "sbproj_view_endconnection", len = 1),
      end_terminator = get_class_from_list(info, "sbproj_view_endterminator", len = c(0, 1))
    )
  class(ret) <- c("sbproj_view_line", "sbproj_view")
  ret
}

#' @export
as_sbproj_view.xml_void_propindex <- function(x, ...) {
  assert_xml_children(x, len = 1)
  ret <- as_sbproj_view(set_nodeclass(xml2::xml_child(x)), ...)
  class(ret) <- c("sbproj_view_index", "sbproj_view")
  ret
}

#' @export
as_sbproj_view.xml_void_propendTerminator <- function(x, ...) {
  assert_xml_children(x, len = 1)
  ret <- as_sbproj_view(set_nodeclass(xml2::xml_child(x)), ...)
  class(ret) <- c("sbproj_view_endterminator", class(ret))
  ret
}

#' @export
as_sbproj_view.xml_object_com.mathworks.bde.elements.lines.terminator.ArrowHead <- function(x, ...) {
  assert_xml_children(x, len = 0)
  ret <- "ArrowHead"
  class(ret) <- c("sbproj_view_terminator_ArrowHead", "sbproj_view")
  ret
}

#' @export
as_sbproj_view.xml_object <- function(x, ...) {
  assert_xml_children(x, len = 0)
  assert_xml_attr_names(x, permutation.of = "idref")
  ret <- xml2::xml_attr(x, attr = "idref")
  class(ret) <- c("sbproj_view_objectref", "sbproj_view")
  ret
}

#' @export
as_sbproj_view.xml_void_propendConnection <- function(x, ...) {
  assert_xml_children(x, len = 1)
  ret <- add_single_element(x, ...)
  class(ret) <- c("sbproj_view_endconnection", class(ret))
  ret
}

#' @export
as_sbproj_view.xml_void_propstartConnection <- function(x, ...) {
  assert_xml_children(x, len = 1)
  ret <- add_single_element(x, ...)
  class(ret) <- c("sbproj_view_startconnection", class(ret))
  ret
}

#' @export
as_sbproj_view.default <- function(x, ...) {
  stop("No as_sbproj_view() method for ", paste(class(x), collapse = ", ")) # nocov
}

# Helper functions ----

assert_xml_children <- function(x, len = NULL, ...) {
  checkmate::assert_class(x, "xml_node")
  if (!is.null(len)) {
    children <- xml2::xml_children(x)
    stopifnot(length(children) == len)
  }
  x
}

#' Verify that an XML node has given attribute names
#'
#' @param x The xml2 node object
#' @inheritDotParams checkmate::check_names
#' @returns x
assert_xml_attr_names <- function(x, ...) {
  checkmate::assert_names(names(xml2::xml_attrs(x)), ...)
  x
}

assert_xml_name <- function(x, name, ...) {
  checkmate::assert_choice(xml2::xml_name(x), choices = name)
  x
}

add_single_element <- function(x, ...) {
  children <- xml2::xml_children(x)
  assert_xml_children(x, len = 1)
  as_sbproj_view(set_nodeclass(children[[1]]), ...)
}

add_multiple_elements <- function(x, ...) {
  children <- xml2::xml_children(x)
  stopifnot(length(children) > 0)
  children <- lapply(X = children, FUN = set_nodeclass)
  lapply(X = children, FUN = as_sbproj_view, ...)
}

#' Get an element that is a class from a list of objects
#'
#' @param x The list to search
#' @param what The class name (passed to `inherts()`)
#' @param what_not The class name(s) it should *not* be
#' @param len The expected length of the return value (may be a vector of
#'   values)
#' @returns A list of elements with the required class, or if
#'   `all(len %in% c(0, 1))`, the value within the list element
get_class_from_list <- function(x, what, what_not = NULL, len = NULL) {
  mask <- vapply(X = x, FUN = inherits, what = what, FUN.VALUE = NA)
  if (length(what_not) > 0) {
    for (current_what_not in what_not) {
      mask <- mask & !vapply(X = x, FUN = inherits, what = current_what_not, FUN.VALUE = NA)
    }
  }
  ret <- x[mask]
  if (!is.null(len) && !(length(ret) %in% len)) {
    stop("Incorrect number of elements, ", length(ret) , " != ", len)
  }
  if (all(len %in% c(0, 1)) && length(ret) == 1) {
    ret <- ret[[1]]
  }
  ret
}

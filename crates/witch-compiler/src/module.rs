use witch_parser::Module;

/// Uses the depth-first search algorithm to deduce the order
/// of transient module dependencies.
pub fn resolve_dependencies(module: Module, result: &mut Vec<Module>) {
    if !result.contains(&module) {
        for m in module.clone().imports.into_values() {
            resolve_dependencies(m, result);
        }
        result.push(module);
    }
}

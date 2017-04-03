use std::collections::{HashMap, HashSet};
use ir;

pub fn opt_translation_unit(tu: &mut ir::TranslationUnit) {
    for decl in &mut tu.declarations {
        opt_declaration(decl);
    }
}

fn opt_declaration(decl: &mut ir::Declaration) {
    match *decl {
        ir::Declaration::ExternFunction { .. } => {}
        ir::Declaration::Function { ref mut bbs, .. } => {
            opt_basic_blocks(bbs);
        }
    }
}

fn opt_basic_blocks(bbs: &mut Vec<ir::BasicBlock>) {
    let opts = [branch_inliner, branch_remover];

    for opt in opts.into_iter() {
        opt(bbs);
    }
}

fn branch_inliner(bbs: &mut Vec<ir::BasicBlock>) {
    let mut directs = HashMap::new();

    fn updater(id: &mut ir::BasicBlockId, directs: &HashMap<ir::BasicBlockId, ir::BasicBlockId>) {
        if let Some(target_id) = directs.get(id) {
            *id = *target_id;
        }
    }

    for bb in bbs.iter() {
        if let ir::Terminator::Br(mut target_id) = bb.terminator {
            if bb.stmts.len() == 0 {
                updater(&mut target_id, &directs);

                for (_, target) in directs.iter_mut() {
                    if *target == bb.id {
                        *target = target_id;
                    }
                }
                directs.insert(bb.id, target_id);
            }
        }
    }

    for bb in bbs.iter_mut() {
        match bb.terminator {
            ir::Terminator::Br(ref mut id) => updater(id, &directs),
            ir::Terminator::BrCond(_, ref mut id1, ref mut id2) => {
                updater(id1, &directs);
                updater(id2, &directs);
            }
            _ => {}
        }
    }
}

fn branch_remover(bbs: &mut Vec<ir::BasicBlock>) {
    let mut succs: HashMap<ir::BasicBlockId, Vec<ir::BasicBlockId>> = HashMap::new();
    for bb in bbs.iter() {
        let succ_list = succs.entry(bb.id).or_insert(Vec::new());
        match bb.terminator {
            ir::Terminator::Br(id) => {
                succ_list.push(id);
            }
            ir::Terminator::Ret(_) => {}
            ir::Terminator::BrCond(_, id1, id2) => {
                succ_list.push(id1);
                succ_list.push(id2)
            }
        }
    }

    let mut opened = Vec::new();
    opened.push(ir::BasicBlockId(0));
    let mut touched = HashSet::<ir::BasicBlockId>::new();

    while opened.len() != 0 {
        let id = opened.pop().unwrap();
        for succ in succs.get(&id).unwrap() {
            if !touched.contains(succ) {
                opened.push(*succ);
            }
        }
        touched.insert(id);
    }

    bbs.retain(|bb| touched.contains(&bb.id));
}

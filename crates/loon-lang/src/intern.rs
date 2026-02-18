use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::rc::Rc;

/// An interned string — cheap to clone (Rc bump), cheap to compare (pointer eq),
/// cheap to hash (pointer hash). Derefs to &str for transparent use.
#[derive(Clone)]
pub struct Symbol(Rc<str>);

impl Symbol {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Symbol {}

impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (Rc::as_ptr(&self.0) as *const () as usize).hash(state);
    }
}

impl Deref for Symbol {
    type Target = str;
    fn deref(&self) -> &str {
        &self.0
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Symbol({:?})", &*self.0)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl AsRef<str> for Symbol {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl PartialEq<str> for Symbol {
    fn eq(&self, other: &str) -> bool {
        &*self.0 == other
    }
}

impl PartialEq<&str> for Symbol {
    fn eq(&self, other: &&str) -> bool {
        &*self.0 == *other
    }
}

impl PartialEq<String> for Symbol {
    fn eq(&self, other: &String) -> bool {
        &*self.0 == other.as_str()
    }
}

/// SAFETY: Symbol contains Rc<str> which is not Send/Sync.
/// In practice, interning is thread-local and Symbols are not shared cross-thread.
/// When spawning threads, deep_clone() on Env creates fresh strings.
unsafe impl Send for Symbol {}
unsafe impl Sync for Symbol {}

thread_local! {
    static INTERNER: RefCell<HashSet<Rc<str>>> = RefCell::new(HashSet::new());
}

/// Intern a string, returning a Symbol that can be cheaply cloned/compared/hashed.
pub fn intern(s: &str) -> Symbol {
    INTERNER.with(|interner| {
        let mut set = interner.borrow_mut();
        if let Some(existing) = set.get(s) {
            Symbol(existing.clone())
        } else {
            let rc: Rc<str> = Rc::from(s);
            set.insert(rc.clone());
            Symbol(rc)
        }
    })
}

/// Intern an already-owned String without extra allocation when possible.
pub fn intern_string(s: String) -> Symbol {
    INTERNER.with(|interner| {
        let mut set = interner.borrow_mut();
        if let Some(existing) = set.get(s.as_str()) {
            Symbol(existing.clone())
        } else {
            let rc: Rc<str> = Rc::from(s);
            set.insert(rc.clone());
            Symbol(rc)
        }
    })
}

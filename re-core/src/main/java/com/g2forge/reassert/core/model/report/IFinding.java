package com.g2forge.reassert.core.model.report;

import org.slf4j.event.Level;

public interface IFinding {
	public default IFinding getInnermostFinding() {
		final IFinding finding = (this instanceof IContextualFinding) ? ((IContextualFinding) this).getFinding() : this;
		if (finding instanceof IContextualFinding) return ((IContextualFinding) finding).getInnermostFinding();
		return finding;
	}

	public Level getLevel();
}

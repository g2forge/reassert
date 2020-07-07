package com.g2forge.reassert.core.model.report;

import org.slf4j.event.Level;

public interface IContextualFinding extends IFinding {
	public IFinding getFinding();

	@Override
	public default Level getLevel() {
		return getFinding().getLevel();
	}
}

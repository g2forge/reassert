package com.g2forge.reassert.contract.model.findings.rule;

import org.slf4j.event.Level;

import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.explain.model.IExplained;

public interface INoticeFinding extends IRuleFinding {
	@Override
	public default Level getLevel() {
		final TermRelation relation = getResult().get();
		switch (relation) {
			case Included:
				return Level.WARN;
			case Excluded:
				return Level.INFO;
			case Unspecified:
				return Level.ERROR;
			default:
				throw new EnumException(TermRelation.class, relation);
		}
	}
}

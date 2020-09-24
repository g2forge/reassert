package com.g2forge.reassert.contract.algorithm.licenseusage.model.finding;

import org.slf4j.event.Level;

import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;

public interface INoticeFinding extends ILicenseUsageFinding {
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

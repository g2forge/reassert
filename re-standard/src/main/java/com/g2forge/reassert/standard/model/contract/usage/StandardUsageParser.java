package com.g2forge.reassert.standard.model.contract.usage;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.api.parser.IParser;
import com.g2forge.reassert.core.model.contract.usage.IUsageApplied;
import com.g2forge.reassert.core.model.contract.usage.UnknownUsage;
import com.g2forge.reassert.core.model.contract.usage.UnspecifiedUsage;

@ReassertLegalOpinion
public class StandardUsageParser implements IParser<IUsageApplied>, ISingleton {
	protected static final StandardUsageParser INSTANCE = new StandardUsageParser();

	public static StandardUsageParser create() {
		return INSTANCE;
	}

	protected StandardUsageParser() {}

	@Override
	public IUsageApplied parse(String text) {
		if (text == null) return UnspecifiedUsage.create();

		try {
			return StandardUsage.valueOf(text);
		} catch (IllegalArgumentException exception) {
			return new UnknownUsage(text);
		}
	}
}

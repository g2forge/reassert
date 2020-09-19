package com.g2forge.reassert.core.api.parser;

import java.util.Collection;

import com.g2forge.reassert.core.model.contract.usage.IUsageApplied;
import com.g2forge.reassert.core.model.contract.usage.UnknownUsage;

public class CompositeUsageParser extends ACompositeParser<IUsageApplied> {
	public CompositeUsageParser(Collection<? extends IParser<? extends IUsageApplied>> parsers) {
		super(parsers);
	}

	@SafeVarargs
	public CompositeUsageParser(IParser<? extends IUsageApplied>... parsers) {
		super(parsers);
	}

	protected IUsageApplied unknown(String text) {
		return new UnknownUsage(text);
	}
}

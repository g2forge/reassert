package com.g2forge.reassert.reassert.convert.contract;

import java.util.function.Function;
import java.util.stream.Stream;

import com.g2forge.alexandria.java.core.error.HError;
import com.g2forge.alexandria.java.core.error.OrThrowable;
import com.g2forge.alexandria.java.core.helpers.HCollector;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.core.api.parser.IParser;
import com.g2forge.reassert.core.model.contract.IContractApplied;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.license.UnknownLicense;
import com.g2forge.reassert.core.model.contract.usage.IUsageApplied;
import com.g2forge.reassert.core.model.contract.usage.UnknownUsage;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Getter(AccessLevel.PROTECTED)
public class ContractParser implements IParser<IContractApplied> {
	protected static <I, O> Function<I, OrThrowable<O>> wrap(Function<? super I, ? extends O> function) {
		return HError.wrap(function);
	}

	protected final IParser<ILicenseApplied> licenseParser;

	protected final IParser<IUsageApplied> usageParser;

	@Override
	public IContractApplied parse(String text) {
		return Stream.<IFunction1<String, IContractApplied>>of(t -> {
			final ILicenseApplied parsed = getLicenseParser().parse(t);
			if (parsed instanceof UnknownLicense) throw new IllegalArgumentException("Text is unknown as a license");
			return parsed;
		}, t -> {
			final IUsageApplied parsed = getUsageParser().parse(t);
			if (parsed instanceof UnknownUsage) throw new IllegalArgumentException("Text is unknown as a contract");
			return parsed;
		}).map(ContractParser::wrap).map(f -> f.apply(text)).collect(HError.collector(() -> new RuntimeException(String.format("Could not parse \"%1$s\" as a contract", text)), true, HCollector.toFirst()));
	}
}

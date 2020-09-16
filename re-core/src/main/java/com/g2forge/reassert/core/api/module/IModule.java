package com.g2forge.reassert.core.api.module;

import java.util.Collection;

import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.parser.IParser;
import com.g2forge.reassert.core.api.scanner.IScanner;
import com.g2forge.reassert.core.api.system.ISystem;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.usage.IUsageApplied;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

public interface IModule {
	@Data
	@Builder(toBuilder = true)
	@RequiredArgsConstructor
	public static class Loaded {
		@Singular
		protected final Collection<ISystem<?>> systems;

		@Singular
		protected final Collection<IScanner> scanners;

		@Singular
		protected final Collection<IParser<ILicenseApplied>> licenseParsers;
		
		@Singular
		protected final Collection<IParser<IUsageApplied>> usageParsers;

		@Singular
		protected final Collection<IDescriber<?>> describers;
	}

	public Loaded load(IContext context);
}
